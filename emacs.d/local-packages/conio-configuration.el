(message "Conio configuration loaded")

(defun conio-docker-command (command)
  "Returns a command to run COMMAND in Docker image of the project"
  (let ((project-root-path (projectile-project-root))
        (project-name (projectile-project-name)))
    (concat "docker run -t -v " project-root-path ":/app -e PYTHON_TEST=1 -e CONIO_ENV=development --network conio_conio_net --link conio_conio_logstash_1:conio_logstash conio_" project-name ":latest " command)))

;; TODO: display output in *python-unittest* buffer
;; TODO: display output with colors?
(defun conio-unittest-current-buffer ()
  "Run unittest on curent buffer in the project container image"
  (interactive)
  (let* ((current-buffer-relative-path (file-relative-name (buffer-file-name) (projectile-project-root)))
         (command (conio-docker-command (concat "/usr/local/bin/python3.5 -m unittest " current-buffer-relative-path)))
         (output (shell-command-to-string command)))
    (message (replace-regexp-in-string "\x0D" "" output))))

(defun conio-pylint-current-buffer ()
  "Run pylint on current buffer in the project container image"
  (interactive)
  (let* ((current-buffer-relative-path (file-relative-name (buffer-file-name) (projectile-project-root)))
         (command (conio-docker-command (concat "/usr/local/bin/pylint -r n --output-format text --msg-template {line}:{column}:{C}:{symbol}:{msg} " current-buffer-relative-path)))
         (output (shell-command-to-string command)))
    (message (replace-regexp-in-string "\x0D" "" output))))

;; TODO pylint: the problem is the output file path which is relative instead of beign the file name, thinking about filtering the output in `~/.pylint` script...
;; TODO pylint: try to define a pycheck checker with real source file avoiding to replace `flycheck_` in input path
(defun conio-python-configuration ()
  "Conio python mode configuration"
  (flycheck-mode +1)

  ;; ;; INSIDE THE CONTAINER
  ;; (setq flycheck-python-pylint-executable (concat (projectile-project-root) "/.pylint"))
  ;; (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; (setq-local flycheck-checkers '(python-pylint))
  ;; (add-to-list 'flycheck-enabled-checkers 'python-pylint)
  ;; (setq-local flycheck-command-wrapper-function
  ;;             (lambda (command)
  ;;               (setf (nth (1- (length command)) command)
  ;;                     (replace-regexp-in-string
  ;;                      (projectile-project-root)
  ;;                      "/app/"
  ;;                      (nth (1- (length command)) command)))
  ;;               (setf (nth (1- (length command)) command)
  ;;                     (replace-regexp-in-string
  ;;                      "flycheck_"
  ;;                      ""
  ;;                      (nth (1- (length command)) command)))
  ;;                 (message "%s" command)
  ;;               command))

  ;; ;; OUTSIDE THE CONTAINER
  ;; (setq flycheck-python-pylint-executable "pylint3")
  ;; (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; (add-to-list 'flycheck-enabled-checkers 'python-pylint)
  ;; (setq-local flycheck-checkers '(python-pylint))

  ;; ENABLE INDENT GUIDES
  (highlight-indent-guides-mode))

(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character))

(use-package python-mode
  :ensure nil
  :init
  (add-hook 'python-mode-hook 'conio-python-configuration))





;; Example of <project-root>/.pylint content

;; #!/bin/bash
;; docker run -t -v /opt/projects/Conio/bitquote/:/app -e PYTHON_TEST=1 -e CONIO_ENV=development --network conio_conio_net --link conio_conio_logstash_1:conio_logstash conio_bitquote:latest /usr/local/bin/pylint $@ | sed 's/^bitquote\///'
;; exit $?
