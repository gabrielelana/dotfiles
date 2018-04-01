(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)
    (package-install 'diminish)))

(require 'use-package)
(setq use-package-always-ensure t)

;; put custom configurations aside
(setq custom-file "~/.emacs.d/custom.el")
(when (not (file-exists-p custom-file))
  (write-region "" nil custom-file))
(load custom-file)

;; start the server if not already started
(load "server")
(unless (server-running-p) (server-start))

;; libraries
(use-package s :ensure t)
(use-package f :ensure t)

;; default configuration
(use-package better-defaults :ensure t)
(use-package scratch :ensure t)

;; themes
(use-package monokai-theme :ensure t :defer t)
(use-package darkokai-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package material-theme :ensure t :defer t)
(use-package apropospriate-theme :ensure t :defer t)
(use-package github-theme :ensure t :defer t)
(use-package mustang-theme
  :ensure t
  :defer t
  :config
  (progn
    ;; flycheck customization
    (set-face-attribute 'flycheck-error nil :box t :underline nil)
    (set-face-attribute 'flycheck-warning nil :box t :underline nil)
    (set-face-attribute 'flycheck-info nil :box t :underline nil)
    ;; mode-line customization
    (set-face-attribute 'mode-line nil :weight 'bold :background "#404040" :foreground "#eeeeec")
    (set-face-attribute 'mode-line-inactive nil :background "#404040" :foreground "#404040")
    (set-face-attribute 'mode-line-buffer-id nil :background "#404040" :foreground "#ff9800")))
(use-package tango-plus-theme
  :ensure t
  :defer t
  :config
  (progn
    ;; flycheck customization
    (set-face-attribute 'flycheck-error nil :box t :underline nil)
    (set-face-attribute 'flycheck-warning nil :box t :underline nil)
    (set-face-attribute 'flycheck-info nil :box t :underline nil)))
;; must load the theme before specific package customization will take place
(load-theme 'mustang t) ;; dark default theme *****
;; (load-theme 'github t) ;; light theme *****
;; (load-theme 'apropospriate-light t) ;; light theme ***
;; (load-theme 'material-light t) ;; light theme ***
;; (load-theme 'tango-plus t) ;; dark theme ****

(use-package popwin
  :ensure t
  :diminish popwin
  :config
  (progn
    (push '("*Occur*" :position bottom :height .3) popwin:special-display-config)
    (push '("*compilation*" :position right :width 80 :noselect t) popwin:special-display-config)
    (popwin-mode 1)))

(use-package drag-stuff
  :ensure t
  :diminish drag " ⇅"
  :config
  (progn
    (setq drag-stuff-except-modes '(org-mode))
    (drag-stuff-define-keys)
    (drag-stuff-global-mode 1)))

(use-package org
  :bind (("C-M-<return>" . org-insert-todo-subheading)))

(use-package expand-region
  :ensure t
  :bind (("M-]" . er/expand-region)
         ("M-[" . er/contract-region)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m n" . mc/mark-next-like-this)
         ("M-\\" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this-dwim)
	 ("C-c m l" . mc/edit-lines))
  :config
  (setq mc/always-run-for-all t))

(use-package visual-regexp
  :ensure t
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode)

(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell-check-startup-files nil)))

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(mode-enabled save))
    (setq flycheck-highlighting-mode 'symbols)
    (setq flycheck-indication-mode nil)
    (setq flycheck-mode-line
          '(:eval
            (pcase flycheck-last-status-change
              (`not-checked " \uf00c")
              (`no-checker " \uf00c[-]")
              (`errored (propertize " \uf00c[!]" 'face '(:foreground "red")))
              (`interrupted " \uf00c[?]")
              (`suspicious " \uf00c[?]")
              (`running " \uf00c[?]")
              (`finished
               (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                      (n-errors (cdr (assq 'error error-counts)))
                      (n-warnings (cdr (assq 'warning error-counts))))
                 (if (or n-errors n-warnings)
                     (propertize (format " \uf00c[%s/%s]" (or n-errors 0) (or n-warnings 0))
                               'face '(:foreground "Tomato"))
                   (propertize " \uf00c" 'face '(:foreground "LimeGreen"))))))))

    (push '("*Flycheck errors*" :position bottom :height .4 :stick t) popwin:special-display-config)))

(use-package magit
  :bind (("C-c g s" . magit-status)
         ("H-s" . magit-status))
  :ensure t)

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine)
         ("H-t" . git-timemachine))
  :ensure t)

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :bind (("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g r" . git-gutter:revert-hunk)
         ("C-c g u" . git-gutter:update-all-windows))
  :init
  (progn
    (global-git-gutter-mode t)
    (custom-set-variables
     '(git-gutter:window-width 2)
     '(git-gutter:added-sign "\uf067")
     '(git-gutter:deleted-sign "\uf068")
     '(git-gutter:modified-sign "\uf054")
     '(git-gutter:hide-gutter nil))
    (let ((git-gutter-default-fg "LightGray"))
      (set-face-foreground 'git-gutter:added git-gutter-default-fg)
      (set-face-attribute 'git-gutter:added nil :height 80)
      (set-face-foreground 'git-gutter:deleted git-gutter-default-fg)
      (set-face-attribute 'git-gutter:deleted nil :height 80)
      (set-face-foreground 'git-gutter:modified git-gutter-default-fg)
      (set-face-attribute 'git-gutter:modified nil :height 80))))

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (progn
    (setq helm-candidate-number-limit 100
          helm-idle-delay 0.0
          helm-input-idle-delay 0.01
          helm-quick-update t
          helm-ff-skip-boring-files t)
    (when (custom-theme-enabled-p 'mustang)
      ;; customization for mustang theme
      (set-face-attribute 'helm-selection nil :background "#3c414c" :foreground "#faf4c6")
      (set-face-attribute 'helm-source-header nil :background "#202020" :foreground "#e2e2e5")
      (set-face-attribute 'helm-candidate-number nil :background "#ff9800" :foreground "#202020")
      (set-face-attribute 'helm-header nil :background "#202020" :foreground "#808080"))
    ;; makes helm and popwin play nice together
    (setq helm-split-window-preferred-function 'ignore)
    (setq display-buffer-function 'popwin:display-buffer)
    (push '("^\*helm.+\*$" :regexp t :position bottom :height .3 :noselect t) popwin:special-display-config)
    (helm-mode))
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-x b" . helm-buffers-list)
   ("M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x)))

(use-package projectile
  :ensure t
  :init
  (progn
    (use-package helm-ag :ensure t)
    (use-package helm-projectile :ensure t))
  :config
  (progn
    ;; TODO: find a better way to configure this
    ;; maybe use `(push directory projectile-globally-ignored-directories)` with a loop?
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "**/elm-stuff")
    (add-to-list 'projectile-globally-ignored-directories "vendor")
    (add-to-list 'projectile-globally-ignored-directories ".tmp")
    (add-to-list 'projectile-globally-ignored-directories ".work")
    (setq projectile-completion-system 'helm)
    (setq projectile-switch-project-action 'helm-projectile-find-file)
    (projectile-global-mode)
    (helm-projectile-on)))

(use-package string-inflection
  :bind (("C-*" . string-inflection-all-cycle))
  :ensure t)

(use-package paredit
  :ensure t
  :diminish (paredit-mode . " (P)")
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

(use-package yaml-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package feature-mode
  :ensure t
  :mode "\\.feature\\'")

;; language: Ruby
(use-package rspec-mode
  :ensure t
  :init
  (setq rspec-spec-command "rspec")
  (setq rspec-key-command-prefix (kbd "C-c t")))

;; language: Elixir
(defun cc/alchemist-do-not-truncate-lines ()
  "Avoid truncate lines in alchemist buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match-p "^*alchemist" (buffer-name buffer))
      (with-current-buffer buffer
        (setq-local truncate-lines nil)))))

(use-package elixir-mode
  :ensure t)

(use-package flycheck-mix
  :ensure t)

(use-package alchemist
  :ensure t
  :diminish alchemist "Alchemist"
  :bind (("C-c a a" . cc/alchemist-do-not-truncate-lines))
  :init
  (add-hook 'elixir-mode-hook #'alchemist-mode)
  :config
  (progn
    (setq alchemist-test-status-modeline nil)
    (when (custom-theme-enabled-p 'mustang)
      ;; customization for mustang theme
      (set-face-attribute 'elixir-attribute-face nil :foreground "#ff9800"))
    (push '("*alchemist test report*" :position right :width 60 :noselect t) popwin:special-display-config)
    (push '("*alchemist help*" :position right :width 60 :noselect t) popwin:special-display-config)
    (push '("*alchemist macroexpand*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist info mode*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist elixirc*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist elixir*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist mix*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (add-hook 'alchemist-mode-hook (lambda()
                                     (flycheck-mode)
                                     (flycheck-mix-setup)))
    (exec-path-from-shell-copy-env "MIX_ARCHIVES")))

;; javascript --- TODO: tern, configure indentation and linting, disable flycheck if eslint executable not found
(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  (progn
    (add-hook 'js-mode-hook (lambda () (setq mode-name "JS")))
    (add-hook 'js2-mode-hook (lambda()
                               (flycheck-mode +1)
                               (when (executable-find "eslint")
                                 (flycheck-select-checker 'javascript-eslint))))
    (when (custom-theme-enabled-p 'mustang)
      ;; customization for mustang theme
      (set-face-attribute 'js2-function-param nil :foreground "#7e8aa2"))
    (setq js-indent-level 2
          js2-basic-offset 2
          js2-strict-trailing-comma-warning nil
          js2-mode-show-parse-errors nil
          js2-mode-show-strict-warnings nil
          js2-strict-missing-semi-warning nil
          js2-use-font-lock-faces t)))

(use-package json-mode
  :ensure t
  :mode "\\.json$")

;; rust --- TODO: racer, clippy, flycheck-rust: navigation between errors doesn't work
(use-package rust-mode
  :ensure t
  :mode ("\\.rs$" . rust-mode)
  :bind (("C-c <tab>" . rust-format-buffer))
  :config
  (progn
    (setq rust-format-on-save t)))

(use-package cargo
  :ensure t
  :diminish cargo-minor-mode
  :commands cargo-minor-mode
  :init
  (progn
    (push '("^\*Cargo.+\*$" :regexp t :position right :width 80 :noselect t) popwin:special-display-config)
    (add-hook 'cargo-process-mode-hook
              (lambda ()
                (with-current-buffer (get-buffer "*Cargo Process*")
                  (setq-local truncate-lines nil)
                  (text-scale-set -1))))
    (add-hook 'rust-mode-hook #'cargo-minor-mode)))

(use-package flycheck-rust
  :ensure t
  :init
  (progn
    (add-hook 'rust-mode-hook #'flycheck-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
    ;; (add-hook 'rust-mode-hook #'yas-minor-mode)
;; (add-hook 'rust-mode-hook #'flyspell-prog-mode)))

;; haskell
;; TODO: take a look at https://github.com/bitemyapp/dotfiles/blob/master/.emacs.d/haskell/hs-lint.el
(use-package haskell-mode
  :ensure t)

(use-package intero
  :hook (haskell-mode . intero-mode)
  :config (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(when (executable-find "hindent")
  (use-package hindent
    :diminish hindent-mode " ↹"
    :hook (haskell-mode . hindent-mode)
    :config
    (setq hindent-reformat-buffer-on-save t)))

;; elm
(use-package elm-mode
  :ensure t
  :mode ("\\.elm'" . elm-mode)
  :config
  (progn
    (push '("*elm-make*" :position right :width 80 :noselect t) popwin:special-display-config)
    (setq elm-indent-offset 4)
    (setq elm-format-on-save t)))

(use-package flycheck-elm
  :ensure t
  :init
  (add-hook 'elm-mode-hook (lambda ()
                             (flycheck-mode)
                             (setq flycheck-check-syntax-automatically '(mode-enabled save))
                             (flycheck-elm-setup))))

;; clojure
(use-package cider)

;; toml
(use-package toml-mode
  :ensure t
  :mode ("\\.toml$" . toml-mode))

;; dockerfile
(use-package dockerfile-mode
  :ensure t)

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc --from markdown_github -t html5 -s"))

;; php
(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode)
  :init
  (use-package phpunit :ensure t)
  (use-package php-functions :ensure nil :load-path "local-packages/")
  (add-hook 'php-mode-hook
            (lambda ()
              (c-set-offset 'case-label '+)
              (modify-syntax-entry ?$ "w" php-mode-syntax-table)
              (local-set-key (kbd "C-c i") 'php-use-at-point)
              (local-set-key (kbd "C-c s") 'php-normalize-use-region)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(mode-enabled save)))))

;; chunkly
(use-package chunkly-mode
  :ensure nil
  :commands (chunkly-mode)
  :load-path "local-packages/"
  :mode ("\\.chunkly/[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.log\\'" . chunkly-mode))

;; functions
(defun cc/load-local-machine-configuration (&optional machine)
  "Load configuration of the current machine or for MACHINE.

The name of the loaded file is `<MACHINE-NAME>-configuration.el`
and must be placed and can be found under
`~/.emacs.d/local-packages` directory"
  (interactive)
  (let* ((machine-name (or machine (system-name)))
         (local-configuration-file (concat "~/.emacs.d/local-packages/" machine-name "-configuration.el")))
    (when (file-exists-p local-configuration-file)
      (load-file local-configuration-file))))

(defun cc/join-with-next-line ()
  "Join this line with the next and fix up whitespace at join."
  (interactive)
  (delete-indentation 1))

(defun cc/comment-or-uncomment-line-or-region ()
  "Comment or uncomment current line."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun cc/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun cc/duplicate-line-or-region (arg)
  "Duplicates the current line or region.

When ARG is less then 0 then the duplicated content will be put
 above the current position, otherwise below.

With \\[universal-argument] prefix the original content will be
 commented.

The point will be left in the original position inside the
 duplicated content."
  (interactive "p")
  ;; TODO extract cc/bounds-of-line-or-region
  (let (start end content (ending-at (point)))
      (if (use-region-p)
          (setq start (save-excursion (goto-char (region-beginning))
                                      (beginning-of-line)
                                      (point))
                end (save-excursion (goto-char (region-end))
                                    (end-of-line)
                                    (point)))
        (setq start (line-beginning-position) end (line-end-position)))
      (setq content (buffer-substring-no-properties start end))
      (delete-region start end)
      (insert (concat content "\n" content))
      (goto-char ending-at)
      (let ((below? (> arg 0)) (comment? (eq (% arg 4) 0)))
        (when comment?
          (if below?
              (comment-region start end)
            (comment-region (+ start (- end start)) (+ end (- end start) 1))))
        (when below? (next-line (count-lines start end))))))

(defun cc/smarter-move-beginning-of-line ()
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line."
  (interactive)
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun cc/open-line-below ()
  "Open a new line below the cursor"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun cc/open-line-above ()
  "Open a new line above the cursor"
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun cc/open-line-here ()
  "Splits the current line where the cursor is"
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

(defun cc/rename-current-buffer-and-file ()
  "Renames the file current buffer is visiting and visit it again."
  (interactive)
  (let* ((filename (buffer-file-name))
         (new-filename (read-file-name "Rename to:" (file-name-directory filename) filename)))
    (if (vc-backend filename)
        (progn
          (vc-state-refresh filename (vc-backend filename))
          (vc-rename-file filename new-filename))
      (progn
        (rename-file filename new-filename)
        (kill-buffer)
        (find-file new-filename)))
    (message "Renamed file %s to %s" filename new-filename)))

(defun cc/delete-current-buffer-and-file ()
  "Kills the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (progn
            (vc-state-refresh filename (vc-backend filename))
            (vc-delete-file filename))
        (progn
          (delete-file filename)
          (kill-buffer)))
      (message "Deleted file %s" filename))))

(require 'ansi-color)
(defun cc/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(defun cc/shell-command-with-color (command)
  "Run COMMAND in shell and apply ansi escapes for colors."
  (interactive)
  (shell-command command)
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun cc/shell-command-on-current-file (command)
  "Run COMMAND in shell appending the current buffer file name to the COMMAND.
Like for example if you want to run `rspec` command with some
options you can do it calling `(cc/shell-command-on-current-file
\"rspec --tty --color\")`."
  (interactive)
  (cc/shell-command-with-color (concat command (buffer-file-name))))

(defun cc/copy-character-from-around (direction)
  "Copy one character from previous or below line starting at point."
  (let (character-to-copy
        (origin (point))
        (column-where-to-copy (current-column)))
    (save-excursion
      (forward-line direction)
      (move-to-column column-where-to-copy)
      (setq character-to-copy (buffer-substring-no-properties (point) (1+ (point)))))
    (insert character-to-copy)))

(defun cc/shell-import-variables-from-dot-env-file (&optional directory-path)
  "Import in current process all environment defined in dot env file."
  (interactive)
  (setq directory-path (if (stringp directory-path) directory-path (projectile-project-root)))
  (let ((dot-env-file-path (concat directory-path "/.env")) env-export-lines env-export)
    (setq env-export-lines (with-temp-buffer
                             (insert-file-contents dot-env-file-path)
                             (split-string (buffer-string) "\n" t)))
    (dolist (line env-export-lines)
      (setq env-export (split-string line "[ =]" nil))
      (setenv (cadr env-export) (car (cddr env-export))))))

(defun cc/copy-character-from-above ()
  "Copy one character from previous non blank line starting above point."
  (interactive)
  (cc/copy-character-from-around -1))

(defun cc/copy-character-from-below ()
  "Copy one character from previous non blank line starting above point."
  (interactive)
  (cc/copy-character-from-around 1))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))

(global-set-key (kbd "H-p") 'cc/open-line-above)
(global-set-key (kbd "H-n") 'cc/open-line-below)
(global-set-key (kbd "H-<return>") 'cc/open-line-here)
(global-set-key (kbd "M-SPC") 'rectangle-mark-mode)
(global-set-key (kbd "H-u") 'cc/copy-character-from-above)
(global-set-key (kbd "H-b") 'cc/copy-character-from-below)
(global-set-key (kbd "C-a") 'cc/smarter-move-beginning-of-line)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-^") 'cc/join-with-next-line)
(global-set-key (kbd "C-;") 'cc/comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-x e") 'cc/eval-and-replace)
(global-set-key (kbd "M-n") 'cc/duplicate-line-or-region)
(global-set-key (kbd "M-p") (lambda (arg) (interactive "p") (cc/duplicate-line-or-region (- arg))))
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c D") 'cc/delete-current-buffer-and-file)
(global-set-key (kbd "C-c R") 'cc/rename-current-buffer-and-file)
(global-set-key (kbd "C-x &") 'end-or-call-macro)

;; global hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'compilation-filter-hook 'cc/colorize-compilation)

;; global configuration
;; always indent after yank
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (not (member major-mode '()))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))
;; use UTF-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; ask `y or n' rather than `yes or no'
(defalias 'yes-or-no-p 'y-or-n-p)
;; don't backup files
(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'make-backup-files nil)
;; don't blink the cursor
(customize-set-variable 'blink-cursor-mode nil)
;; when scroll to the bottom/top then place the cursor to the very last/first line
(customize-set-variable 'scroll-error-top-bottom t)
;; invoke commands that use minibuffers even while the minibuffer window is active
(setq enable-recursive-minibuffers t)
;; highlight current line
(global-hl-line-mode +1)
;; replace region when you type something and a region is active
(delete-selection-mode)
;; diplay line and column number in modeline
(line-number-mode t)
(column-number-mode t)

;; shell-script-mode default configurations
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; local configuration
(cc/load-local-machine-configuration)

;; appearance
(setq visible-bell nil)
(setq inhibit-splash-screen t)
(set-frame-font "PragmataPro Mono 14")

;; enable some "dangerous" commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
