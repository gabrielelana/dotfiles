(require 's)
(require 'dash)

;;; TODO: what to do if spaceland is not installed?
;;; TODO: document what is a `use region`
;;; TODO: can create use block if not present
;;; TODO: add erts tests

;;; TODO: use (`unqualified name` or `u-name`), (`qualified name` or `q-name`) and (`fully qualified name` or `fq-name`)
;;; TODO: do not use `use` but `import` as language
;;; TODO: move from classname to generally names of things (class, traits, interfaces, functions)
;;; TODO: regular expressions as private constants
;;; TODO: php-remove-namespace-block
;;; TODO: php-rename-variable-in-scope

;;; TODO: check if php is available
;;; TODO: check if spaceland is available
;;; TODO: extract spaceland-run
;;; TODO: extract configuration spaceland-cache-file-name
;;; TODO: extract configuration spaceland-cache-directory
(defun php-known-classes ()
  "List of known classes. Requires spaceland"
  (let ((project-root (projectile-project-root)))
    (split-string (shell-command-to-string (format "php %s/vendor/bin/spaceland locate:classes --cache-file=%s/.spaceland.cache" project-root project-root)))))

(defun php-classname-exists? (classname &optional known-classes)
  "Check if fully qualified CLASSNAME is declared somewhere."
  (let ((classes (or known-classes (php-known-classes))))
   (-contains? (php-known-classes) classname)))

(defun php-fully-qualified-classname-in-current-namespace (classname)
  "Returns fully qualified name of CLASSNAME in current namespace.

Either CLASSNAME is already a fully qualified name or CLASSNAME
will be completed in the current namespace"
  (if (php-fully-qualified-classname? classname)
      classname
    (php-join-class-paths (php--current-namespace) classname)))

;; TODO I guess it can be done more cleanly... but it works
(defun php-replace-array-with-square-brakets ()
  "Replace ~array()~ with ~[]~ in whole buffer or region"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "\\_<array(" nil t)
        (backward-char)
        (let ((beginning-of-array (point)))
          (forward-sexp)
          (delete-char -1)
          (insert "]")
          (goto-char beginning-of-array)
          (delete-char 1)
          (insert "[")
          (backward-word)
          (kill-word 1))))))

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

(defun php-import-classname-at-point ()
  "Include the qualified name at point in use block keeping it sorted."
  (interactive)
  (let ((classname (php-classname-at-point)) (known-classes (php-known-classes)))
    ;; the classname is available in the current namespace
    (if (php-classname-exists? (php-fully-qualified-classname-in-current-namespace classname) known-classes)
        ;; TODO: better message, class <CLASSNAME> is already imported
        (message "nothing to be done")
      ;; the classname is available through one of the imported names
      (if (php--classname-exists-in-imported-names? classname (php--imported-names) known-classes)
          ;; TODO: better message, class <CLASSNAME> is already imported
          (message "nothing to be done")
        (php--import-classname classname)))))

(defun php--classname-exists-in-imported-names? (classname imported-names &optional known-classes)
  "Check if CLASSNAME can be resolved in IMPORTED-NAMES."
  (let ((classes (or known-classes (php-known-classes))))
    (cond ((php-unqualified-classname? classname)
           ;; if CLASSNAME is an unqualified name then one of the IMPORTED-NAMES must be the fully qualified name of that class
           (--some? (equal (php-classname-of it) classname) imported-names))
          ((php-qualified-classname? classname)
           ;; if CLASSNAME is a partially qualified name then we need to check if CLASSNAME can be resolved in each IMPORTED-NAMES
           (--some? (php-classname-exists? (php-join-class-paths classname it) classes) imported-names)))))

;;; TODO: support aliases `use com\tutorialspoint\ClassC as C;`
;;; TODO: support functions `use function com\tutorialspoint\fn_a;`
;;; TODO: support const `use const com\tutorialspoint\ConstA;`
;;; TODO: support composition `use com\tutorialspoint\{ClassA, ClassB, ClassC as C};`
(defun php--imported-names ()
  "Returns a list of used namespaces through use statements."
  (-let (((start-at end-at) (php-locate-use-region))
         (use-statement-re "^use[[:blank:]]*\\([_a-zA-Z0-9\\\\]\+\\)[[:blank:]]*;"))
    (if (equal start-at end-at)
        nil
      (->> (s-lines (buffer-substring-no-properties start-at end-at))
           (-filter (lambda (s) (s-matches? use-statement-re s)))
           (-map (lambda (s) (replace-regexp-in-string use-statement-re "\\1" s)))))))

(defun php--import-classname (classname)
  (let* ((to-import (php--choose-fully-qualified-classname classname))
         (to-leave (php-classname-of to-import)))
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward classname nil t)
        (replace-match to-leave nil t))
      (php--use-classname to-import))))

(defun php--choose-fully-qualified-classname (classname)
  (let ((candidates (--filter (s-ends-with? classname it) (php-known-classes))))
    (if (eq (length candidates) 1)
        (car candidates)
      (completing-read
       "Choose the fully qualified class name: "
       candidates
       nil
       nil
       nil))))

(defun php--use-classname (to-use)
  (message "(php--use-classname %S)" to-use)
  (save-excursion
    (-let (((start-at end-at) (php-locate-use-region)))
      (goto-char end-at)
      (newline)
      (when (equal start-at end-at)
        ;; there's no use region so let's space more
        (newline))
      (insert (format "use %s;" to-use))
      (indent-for-tab-command)
      (php--normalize-use-region)
      (save-buffer))))

(defun php--normalize-use-region ()
  "Normalize the block of code where ~use~ statements are."
  (interactive)
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

(defun php--current-namespace ()
  (save-excursion
    (let ((namespace-match (search-backward-regexp "^namespace[[:blank:]]\+\\(.*\\)[;{]$" nil t)))
      (if namespace-match
          (s-trim (match-string-no-properties 1))
        "\\"))))

(defun php-locate-use-region ()
  "Returns (START-POSITION END-POSITION) of use region"
  (-first 'identity
           (list
            (php--locate-use-region)
            (php--locate-namespace-declaration)
            (php--locate-open-tag))))

(defun php--locate-use-region ()
  (save-excursion
    (end-of-buffer)
    (while (search-backward-regexp "^\\s-*use\\s-\+.\+$" nil t))
    (if (eobp)
        nil
      (let ((start-at (match-beginning 0)))
        (while (search-forward-regexp "^\\s-*use\\s-\+.\+$" nil t))
        (list start-at (match-end 0))))))

(defun php--locate-namespace-declaration ()
  (save-excursion
    (beginning-of-buffer)
    (let ((match (search-forward-regexp "^namespace\\s-\+.\+[;{]\\s-*$" nil t)))
      (if match
          (list (match-end 0) (match-end 0))
        nil))))

(defun php--locate-open-tag ()
  (save-excursion
    (beginning-of-buffer)
    (let ((match (search-forward-regexp "^<\\?php.*$")))
      (if match
          (list (match-end 0) (match-end 0))
        nil))))

;; TODO: check that the output is a valid fully qualified class name
(defun php-classname-at-point ()
  (save-excursion
    (let (start-at end-at)
      (skip-chars-backward "_\\\\a-zA-Z0-9")
      (setq start-at (point))
      (skip-chars-forward "_\\\\a-zA-Z0-9")
      (setq end-at (point))
      (buffer-substring-no-properties start-at end-at))))

(defun php-unqualified-classname? (classname)
  (and (not (php-fully-qualified-classname? classname))
       (not (php-qualified-classname? classname))))

(defun php-fully-qualified-classname? (classname)
  (s-starts-with? "\\" classname))

(defun php-qualified-classname? (classname)
  (and (s-contains? "\\" classname) (not (php-fully-qualified-classname? classname))))

(defun php-classname? (classname)
  (let ((case-fold-search nil))
    (s-matches? "^\\\\?[_A-Z][_a-zA-Z0-9]\+\\(\\\\[_A-Z][_a-zA-Z0-9]\+\\)*$" classname)))

;;; TODO: rename to php--unqualified-name-of (name)
(defun php-classname-of (classname)
  (car (reverse (s-split "\\\\" classname))))

;;; TODO: rename to php--root-name-of (name)
(defun php-rootname-of (classname)
  (car (s-split "\\\\" classname)))

;;; TODO: rename to php--namespace-of (name)
(defun php-namespace-of (classname)
  (if (or (php-fully-qualified-classname? classname) (php-qualified-classname? classname))
      (s-join "\\" (reverse (cdr (reverse (s-split "\\\\" classname)))))
    nil))

(defun php-qualified-classname-of (classname)
  (s-chop-prefix "\\" classname))

;;; TODO: rename php-join-names
(defun php-join-class-paths (namespace classname)
  (s-join "\\" (list namespace (s-chop-prefix "\\" classname))))

(provide 'php-functions)
