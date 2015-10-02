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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dark t)
(setq visible-bell nil)
(set-frame-font "PragmataPro 14")
