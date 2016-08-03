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

(use-package better-defaults)

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell-check-startup-files nil)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter
  :bind (("C-c h n" . git-gutter:next-hunk)
         ("C-c h p" . git-gutter:previous-hunk))
  :config
  (progn
    (custom-set-variables
     '(git-gutter:window-width 2)
     '(git-gutter:modified-sign "\uf0ad")
     '(git-gutter:added-sign "\uf005")
     '(git-gutter:deleted-sign "\uf004"))
    (when (custom-theme-enabled-p 'darkokai)
      (let ((git-gutter-font "Pragmata Pro 10")
            (darkokai-green "#63DE5D")
            (darkokai-yellow-l "#FFF7A8")
            (darkokai-red-d "#F70057")
            (darkokai-bg "#242728"))
        (set-face-font 'git-gutter:added git-gutter-font)
        (set-face-background 'git-gutter:added darkokai-bg)
        (set-face-foreground 'git-gutter:added darkokai-green)
        (set-face-font 'git-gutter:modified git-gutter-font)
        (set-face-background 'git-gutter:modified darkokai-bg)
        (set-face-foreground 'git-gutter:modified darkokai-yellow-l)
        (set-face-font 'git-gutter:deleted git-gutter-font)
        (set-face-background 'git-gutter:deleted darkokai-bg)
      (set-face-foreground 'git-gutter:deleted darkokai-red-d)))
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
      ;; helm customization
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
    (projectile-global-mode)
    (helm-projectile-on)))

(use-package mustang-theme
  :ensure t
  :defer t
  :config
  (progn
    ;; mode-line customization
    (set-face-attribute 'mode-line nil :weight 'bold :background "#404040" :foreground "#eeeeec")
    (set-face-attribute 'mode-line-inactive nil :background "#404040" :foreground "#404040")
    (set-face-attribute 'mode-line-buffer-id nil :background "#404040" :foreground "#ff9800")))

(use-package subatomic-theme
  :ensure t
  :defer t
  :config
  (progn
    (setq subatomic-high-contrast t)
    (setq subatomic-more-visible-comment-delimiters t)))

(use-package monokai-theme :ensure t :defer t)

(use-package darkokai-theme :ensure t :defer t)

(load-theme 'darkokai t)



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

;; global hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; place all backcup files in one directory to avoid clutter current project
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; appearance
(setq visible-bell nil)
(setq inhibit-splash-screen t)
(set-frame-font "PragmataPro 14")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; put custom configurations aside
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(put 'set-goal-column 'disabled nil)
