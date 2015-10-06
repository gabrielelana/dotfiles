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
    helm
    helm-ag
    flymake
    php-mode))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

;; Ensure default-packages are all installed
(dolist (p default-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

(require 'smartparens-config)
(smartparens-mode 1)

(require 'helm-config)
(helm-mode 1)

;; Themes
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

;; Custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
