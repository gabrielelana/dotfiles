(message "Conio configuration loaded")

;; ;; python
;; (use-package highlight-indent-guides
;;   :ensure t
;;   :init
;;   (setq highlight-indent-guides-method 'character))

;; (defun conio-docker-command (command)
;;   "Returns a command to run COMMAND in Docker image of the project"
;;   (let ((project-root-path (projectile-project-root))
;;         (project-name (projectile-project-name)))
;;     (concat "docker run -t -v " project-root-path ":/app -e PYTHON_TEST=1 -e CONIO_ENV=development --network conio_conio_net --link conio_conio_logstash_1:conio_logstash conio_" project-name ":latest " command)))

;; (use-package python-mode
;;   :ensure nil
;;   :init
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (flycheck-mode +1)
;;               (setq flycheck-check-syntax-automatically '(mode-enabled save))
;;               (setq flycheck-python-pylint-executable (conio-docker-command "/usr/local/bin/pylint"))
;;               (highlight-indent-guides-mode))))



;; (defun unittest-current-buffer ()
;;   (interactive)
;;   (let* ((project-root (projectile-project-root))
;;          (current-buffer-relative-path (file-relative-name (buffer-file-name) project-root))
;;          (command (concat "docker run -t -v " project-root ":/app -e PYTHON_TEST=1 -e CONIO_ENV=development --network conio_conio_net --link conio_conio_logstash_1:conio_logstash conio_bitquote:latest /usr/local/bin/python3.5 -m unittest " current-buffer-relative-path))
;;          (output (shell-command-to-string command)))
;;     (message (replace-regexp-in-string "\x0D" "" output))))
