(require 's)
(require 'dash)

;; TODO support use of functions
;; TODO regular expressions as private constants
;; TODO php-remove-namespace-block
;; TODO php-rename-variable-in-scope
;; TODO php-use-at-point choosing namespace when TAGS are available

;; TODO I guess it can be done more cleanly... but it works
(defun php-replace-array-with-square-brakets ()
  "Replace ~array()~ with ~[]~ in whole buffer or region"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward "array(" nil t)
      (backward-char)
      (let ((beginning-of-array (point)))
        (forward-sexp)
        (delete-char -1)
        (insert "]")
        (goto-char beginning-of-array)
        (delete-char 1)
        (insert "[")
        (backward-word)
        (kill-word 1)))))

;; TODO fail if phpunit is not loaded?
(defun phpunit-current-file-relative-path ()
  "Current test file path relative to project root directory."
  (s-chop-prefix (phpunit-get-root-directory) buffer-file-name))

;; TODO fail if phpunit is not loaded?
(defun phpunit-current-function ()
  "Launch PHPUnit on current function."
  (interactive)
  (exec-path-from-shell-copy-env "APPLICATION_ENV")
  (let ((args (format " --filter '%s::%s' %s"
                      (phpunit-get-current-class)
                      (phpunit-get-current-test)
                      (phpunit-current-file-relative-path))))
    (phpunit-run args)))

(defun php-use-at-point ()
  "Include the qualified name at point in use block keeping it sorted."
  (interactive)
  (let ((classname (php-classname-at-point)) to-use to-leave)
    (if (php-unqualified-classname? classname)
        (message "nothing to be done")
      (if (php-fully-qualified-classname? classname)
          (setq to-use (php-qualified-classname-of classname)
                to-leave (php-classname-of classname))
        (setq to-use (php-fully-qualified-classname-of classname)
              to-leave (php-classname-of classname)))
      (save-excursion
        (beginning-of-buffer)
        (while (search-forward classname nil t)
          (replace-match to-leave nil t))
        (php-use-classname to-use)))))

(defun php-used-namespaces ()
  "Returns a list of used namespaces through use statements."
  (-let (((start-at end-at) (php-locate-use-region))
         (use-statement-re "^use[[:blank:]]*\\([_a-zA-Z0-9\\\\]\+\\)[[:blank:]]*;"))
    (->> (s-lines (buffer-substring-no-properties start-at end-at))
         (-filter (lambda (s) (s-matches? use-statement-re s)))
         (-map (lambda (s) (replace-regexp-in-string use-statement-re "\\1" s))))))

(defun php-fully-qualified-classname-of (classname)
  "Returns the fully qualified name of CLASSNAME in current namespace."
  (-if-let (namespace (-first (lambda (s) (s-equals? (php-rootname-of classname) (php-classname-of s)))
                              (php-used-namespaces)))
      (php-join-classname namespace (php-classname-of classname))
    (php-join-classname (php-current-namespace) classname)))


(defun php-use-classname (to-use)
  (save-excursion
    (-let (((start-at end-at) (php-locate-use-region)))
      (goto-char end-at)
      (newline)
      (insert (format "use %s;" to-use))
      (indent-for-tab-command)
      (php-normalize-use-region))))

(defun php-normalize-use-region ()
  (php--sort-lines-in-use-region)
  (php--remove-duplicated-lines-in-use-region)
  (php--remove-empty-lines-in-use-region))

(defun php--sort-lines-in-use-region ()
  (-let (((start-at end-at) (php-locate-use-region)))
    (sort-lines nil start-at end-at)))

(defun php--remove-duplicated-lines-in-use-region ()
  (-let (((start-at end-at) (php-locate-use-region)))
    (delete-duplicate-lines start-at end-at)))

(defun php--remove-empty-lines-in-use-region ()
  (-let (((start-at end-at) (php-locate-use-region)))
    (delete-matching-lines "^\\s-*$" start-at end-at)))

(defun php-current-namespace ()
  (save-excursion
    (let ((namespace-match (search-backward-regexp "^namespace[[:blank:]]\+\\(.*\\)[;{]$" nil t)))
      (if namespace-match
          (s-trim (match-string-no-properties 1))
        ""))))

;; TODO: what if there isn't an use statement yet?
(defun php--locate-use-region ()
  (save-excursion
    (while (search-backward-regexp "^\\s-*use\\s-\+.\+$" nil t))
    (let ((start-at (match-beginning 0)))
      (while (search-forward-regexp "^\\s-*use\\s-\+.\+$" nil t))
      (let ((end-at (match-end 0)))
        (if (> end-at start-at 0)
            (list start-at end-at)
          (php--locate-namespace-declaration))))))

(defun php--locate-namespace-declaration ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "^namespace\\s-\+.\+[;{]\\s-*$" nil t)
    (let ((start-at (match-beginning 0))
          (end-at (match-ending 0)))
      (if (> end-at start-at 0)
          (list start-at end-at)
        (php--locate-open-tag)))))

(defun php--locate-open-tag ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward-buffer "^<\?php.*$" nil t)
    (let ((start-at (match-beginning 0))
          (end-at (match-ending 0)))
      (list start-at end-at))))

;; TODO: check that the output is a valid fully qualified class name
(defun php-classname-at-point ()
  (save-excursion
    (let (start-at end-at)
      (skip-chars-backward "_\\\\a-zA-Z0-9")
      (setq start-at (point))
      (skip-chars-forward "_\\\\a-zA-Z0-9")
      (setq end-at (point))
      (buffer-substring-no-properties start-at end-at))))

(defun php-classname? (classname)
  (let ((case-fold-search nil))
    (s-matches? "^\\\\?[_A-Z][_a-zA-Z0-9]\+\\(\\\\[_A-Z][_a-zA-Z0-9]\+\\)*$" classname)))

(defun php-fully-qualified-classname? (classname)
  (s-starts-with? "\\" classname))

(defun php-qualified-classname? (classname)
  (and (s-contains? "\\" classname) (not (php-fully-qualified-classname? classname))))

(defun php-unqualified-classname? (classname)
  (and (not (php-fully-qualified-classname? classname)) (not (php-qualified-classname? classname))))

;; TODO: use pipe operator?
(defun php-classname-of (classname)
  (car (reverse (s-split "\\\\" classname))))

(defun php-rootname-of (classname)
  (car (s-split "\\\\" classname)))

(defun php-namespace-of (classname)
  (if (or (php-fully-qualified-classname? classname) (php-qualified-classname? classname))
      (s-join "\\" (reverse (cdr (reverse (s-split "\\\\" classname)))))
    nil))

(defun php-qualified-classname-of (classname)
  (s-chop-prefix "\\" classname))

;; TODO php-join-classnames (namespace classnames)
(defun php-join-classname (namespace classname)
  (s-join "\\" (list namespace (s-chop-prefix "\\" classname))))

(provide 'php-functions)
