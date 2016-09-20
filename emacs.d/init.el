(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)

;; put custom configurations aside
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; start the server if not already started
(load "server")
(unless (server-running-p) (server-start))

;; libraries
(use-package s :ensure t)

;; default configuration
(use-package better-defaults :ensure t)

;; themes
(use-package monokai-theme :ensure t :defer t)
(use-package darkokai-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package mustang-theme
  :ensure t
  :defer t
  :config
  (progn
    ;; flycheck custimization
    (set-face-attribute 'flycheck-error nil :box t :underline nil)
    (set-face-attribute 'flycheck-warning nil :box t :underline nil)
    (set-face-attribute 'flycheck-info nil :box t :underline nil)
    ;; mode-line customization
    (set-face-attribute 'mode-line nil :weight 'bold :background "#404040" :foreground "#eeeeec")
    (set-face-attribute 'mode-line-inactive nil :background "#404040" :foreground "#404040")
    (set-face-attribute 'mode-line-buffer-id nil :background "#404040" :foreground "#ff9800")))
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package sublime-themes :ensure t :defer t)
(use-package tango-plus-theme :ensure t :defer t)
(use-package material-theme :ensure t :defer t)
(use-package dracula-theme :ensure t :defer t)
(use-package soft-stone-theme :ensure t :defer t)
;; must load the theme before specific package customization will take place
(load-theme 'mustang t)
;; (load-theme 'tango-plus t) ;; ****
;; (load-theme 'material-light t) ;; ***

(use-package popwin
  :ensure t
  :diminish popwin
  :config
  (progn
    (push '("*Occur*" :position bottom :height .3) popwin:special-display-config)
    (popwin-mode 1)))

(use-package drag-stuff
  :ensure t
  :diminish drag " ⇅"
  :config
  (progn
    (setq drag-stuff-except-modes '(org-mode))
    (drag-stuff-global-mode 1)))

(use-package expand-region
  :ensure t
  :bind (("M-]" . er/expand-region)
         ("M-[" . er/contract-region)))

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

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :bind (("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g u" . git-gutter:update-all-windows))
  :config
  (progn
    (custom-set-variables
     '(git-gutter:window-width 2)
     '(git-gutter:added-sign "\uf067")
     '(git-gutter:deleted-sign "\uf068")
     '(git-gutter:modified-sign "\uf054")
     '(git-gutter:hide-gutter t))
    (let ((git-gutter-default-fg "LightGray"))
      (set-face-foreground 'git-gutter:added git-gutter-default-fg)
      (set-face-attribute 'git-gutter:added nil :height 80)
      (set-face-foreground 'git-gutter:deleted git-gutter-default-fg)
      (set-face-attribute 'git-gutter:deleted nil :height 80)
      (set-face-foreground 'git-gutter:modified git-gutter-default-fg)
      (set-face-attribute 'git-gutter:modified nil :height 80))
    (global-git-gutter-mode t)))

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
      ;; helm customization for mustang theme
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
    (setq projectile-completion-system 'helm)
    (setq projectile-switch-project-action 'helm-projectile-find-file)
    (projectile-global-mode)
    (helm-projectile-on)))

(use-package yaml-mode
  :ensure t)

(use-package feature-mode
  :ensure t
  :mode "\\.feature\\'")

;; language: Elixir
(defun cc/alchemist-do-not-truncate-lines ()
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match-p "^*alchemist" (buffer-name buffer))
      (with-current-buffer buffer
        (setq-local truncate-lines nil)))))

(use-package elixir-mode
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
    (push '("*alchemist test report*" :position right :width 60 :noselect t) popwin:special-display-config)
    (push '("*alchemist help*" :position right :width 60 :noselect t) popwin:special-display-config)
    (push '("*alchemist macroexpand*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist elixirc*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist elixir*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist mix*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (exec-path-from-shell-copy-env "MIX_ARCHIVES")))

;; javascript --- TODO: tern, configure indentation and linting, disable flycheck if eslint executable not found
(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  (progn
    (add-hook 'js-mode-hook (lambda () (setq mode-name "JS")))
    (add-hook 'js2-mode-hook (lambda()
                               (flycheck-mode t)
                               (when (executable-find "eslint")
                                 (flycheck-select-checker 'javascript-eslint))))
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

(use-package toml-mode
  :ensure t
  :mode ("\\.toml$" . toml-mode))

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
  :mode ("\\.php\\'" . php-mode))

;; chunkly
(use-package chunkly-mode
  :ensure nil
  :commands (chunkly-mode)
  :load-path "local-packages/"
  :mode ("\\.chunkly/[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.log\\'" . chunkly-mode))

;; functions
(defun cc/join-with-next-line ()
  "Join this line with the next and fix up whitespace at join."
  (interactive)
  (delete-indentation 1))

(defun cc/toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun cc/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(require 's)
(defun cc/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated. If the
argument is negative the line/region will be duplicated above the current
position."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let* ((region (buffer-substring-no-properties beg end))
           (number-of-copies (abs arg))
           (content (s-repeat number-of-copies (s-concat region "\n")))
           (number-of-lines (- (* arg (length (s-lines content))) 1))
           (op (if (< 0 arg) '+ '-)))
      (goto-char beg)
      (insert content)
      (goto-char origin)
      (if (< 0 arg)
          (next-line number-of-lines)))))

(global-set-key (kbd "H-p") 'previous-buffer)
(global-set-key (kbd "H-n") 'next-buffer)
(global-set-key (kbd "C-c l") 'org-store-link) ; capture link at point
(global-set-key (kbd "C-^") 'cc/join-with-next-line)
(global-set-key (kbd "C-;") 'cc/toggle-comment-on-line)
(global-set-key (kbd "M-n") 'cc/duplicate-current-line-or-region)
(global-set-key (kbd "M-p") (lambda (arg) (interactive "p") (cc/duplicate-current-line-or-region (- arg))))
(global-set-key (kbd "M-o") 'other-window)

;; global hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; global configuration
;; indent after yank
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (not (member major-mode '()))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))
;; place all backcup files in one directory to avoid clutter current project
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
;; invoke commands that use minibuffers even while the minibuffer window is active
(setq enable-recursive-minibuffers t)
;; highlight current line
(global-hl-line-mode +1)
;; replace region when you type something and a region is active
(delete-selection-mode)
;; diplay line and column number in modeline
(line-number-mode t)
(column-number-mode t)

;; appearance
(setq visible-bell nil)
(setq inhibit-splash-screen t)
(set-frame-font "PragmataPro 14")
