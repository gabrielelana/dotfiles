(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(defvar default-packages
  '(better-defaults
    f
    s
    let-alist
    expand-region
    elixir-mode
    alchemist
    smartparens
    sublime-themes
    mustang-theme
    rainbow-mode
    exec-path-from-shell
    helm
    helm-ag
    helm-ls-git
    flymake
    yaml-mode
    transpose-frame
    php-mode))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; activate installed packages
(package-initialize)

;; ensure default-packages are all installed
(dolist (p default-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; general configuration
(setq inhibit-startup-message t)
(delete-selection-mode 1)

(defun copy-line (arg)
  "Copy lines in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;; bindings to kill
(global-unset-key (kbd "C-k"))
(global-set-key (kbd "C-k f w") 'kill-word)
(global-set-key (kbd "C-k b w") 'backward-kill-word)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-k C-l") 'kill-whole-line)
(global-set-key (kbd "C-k C-k") 'kill-line)
(global-set-key (kbd "C-k s") 'sp-kill-sexp)
(global-set-key (kbd "C-k f l") 'kill-line)
(global-set-key (kbd "C-k r") 'kill-region)

;; bindings to copy
(global-set-key (kbd "C-c c l") 'copy-line)

;; bindings
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'hippie-expand)

;; smartparens
(require 'smartparens-config)
(smartparens-mode 1)
(setq sp-highlight-pair-overlay nil)

;; helm
(require 'helm-config)
; (helm-autoresize-mode 1) don't know why it doesn't work
(setq helm-autoresize-max-height 30)
(setq helm-split-window-in-side-p t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c C-p") 'helm-ls-git-ls)
(global-set-key (kbd "C-c C-a") 'helm-ag-project-root)
(helm-mode 1)
(ido-mode -1)

;; alchemist
;; find a better way of doing this, also based on this point to the appropriate elixir sources
(setenv "MIX_ARCHIVES" "/home/coder/.kiex/mix/archives/elixir-1.1.1")
(setq alchemist-test-status-modeline nil)

(defun cc/elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "->" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(:add cc/elixir-do-end-close-action)
                 :actions '(insert)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(:add cc/elixir-do-end-close-action)
                 :actions '(insert)))

;; Display alchemist buffers always at the bottom
;; Source: http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos (or "*alchemist test report*"
;;                             "*alchemist mix*"
;;                             "*alchemist help*"
;;                             "*alchemist elixir*"
;;                             "*alchemist elixirc*"))
;;                (display-buffer-reuse-window display-buffer-in-side-window)
;;                (reusable-frames . visible)
;;                (side            . right)
;;                (window-width    . 0.5)))

(defun cc/alchemist-mode-hook ()
  ;; TODO: yas/minor-mode
  (alchemist-mode)
  (smartparens-mode))

(add-hook 'elixir-mode-hook 'cc/alchemist-mode-hook)


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; initialize exec-path taking values from $PATH
(exec-path-from-shell-initialize)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda()
             (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent)))

;; themes
(setq visible-bell nil)
(set-frame-font "PragmataPro 14")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'mustang t)
;; helm customization for mustang
(set-face-attribute 'helm-selection nil :background "#3c414c" :foreground "#faf4c6")
(set-face-attribute 'helm-source-header nil :background "#202020" :foreground "#e2e2e5")
(set-face-attribute 'helm-candidate-number nil :background "#ff9800" :foreground "#202020")
(set-face-attribute 'helm-header nil :background "#202020" :foreground "#808080")
;; mode-line customization for mustang
(set-face-attribute 'mode-line nil :weight 'bold :background "#404040" :foreground "#eeeeec")
(set-face-attribute 'mode-line-inactive nil :background "#404040" :foreground "#404040")
(set-face-attribute 'mode-line-buffer-id nil :background "#404040" :foreground "#ff9800")

;; custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
