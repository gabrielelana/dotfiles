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

(use-package better-defaults :ensure t)

(use-package monokai-theme :ensure t :defer t)
(use-package darkokai-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package mustang-theme
  :ensure t
  :defer t
  :config
  (progn
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
  (popwin-mode 1))

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

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :bind (("C-c h n" . git-gutter:next-hunk)
         ("C-c h p" . git-gutter:previous-hunk)
         ("C-c h u" . git-gutter:update-all-windows))
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

(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t
  :diminish alchemist "Alchemist"
  :config
  (progn
    (setq alchemist-test-status-modeline nil)
    (push '("*alchemist test report*" :position right :width 60 :noselect t) popwin:special-display-config)
    (push '("*alchemist help*" :position right :width 60 :noselect t) popwin:special-display-config)
    (push '("*alchemist macroexpand*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist elixirc*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist elixir*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (push '("*alchemist mix*" :position bottom :width .4 :noselect t) popwin:special-display-config)
    (add-hook 'alchemist-test-report-mode-hook (lambda ()
                                                 (toggle-truncate-lines)
                                                 (text-scale-set -1)))
    (exec-path-from-shell-copy-env "MIX_ARCHIVES")))

;; functions
(defun cc/join-with-next-line ()
  "Join this line with the next and fix up whitespace at join."
  (interactive)
  (delete-indentation 1))

;; keybindings
(global-set-key (kbd "C-c l") 'org-store-link) ; capture link at point
(global-set-key (kbd "C-^") 'cc/join-with-next-line)
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
