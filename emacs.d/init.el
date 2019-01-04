(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;; install and configure use-package
(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)
    (package-install 'diminish)))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;; configure use-package
(setq use-package-always-ensure t)

;;; put custom configurations aside
(setq custom-file "~/.emacs.d/custom.el")
(when (not (file-exists-p custom-file))
  (write-region "" nil custom-file))
(load custom-file)

;;; start the server if not already started
(load "server")
(unless (server-running-p) (server-start))

;;; libraries
(use-package s)
(use-package f)
(use-package uuidgen)
(use-package request)

;;; default configuration
(use-package better-defaults)
(use-package scratch)

;;; themes
(use-package monokai-theme :defer t)
(use-package doom-themes :defer t)
(use-package dracula-theme :defer t)
(use-package challenger-deep-theme :defer t)
(use-package nord-theme
  :defer t
  :after (flycheck flyspell)
  :init
  (setq nord-region-highlight "snowstorm")
  (setq nord-comment-brightness 20)
  :config
  (set-face-attribute 'flycheck-error nil :box '(:line-width 1 :color "snowstorm" :style nil) :underline nil)
  (set-face-attribute 'flycheck-warning nil :box '(:line-width 1 :color "snowstorm" :style nil) :underline nil)
  (set-face-attribute 'flycheck-info nil :box '(:line-width 1 :color "snowstorm" :style nil) :underline nil)
  (set-face-attribute 'flyspell-duplicate nil :box nil :underline '(:color "steelblue2" :style line))
  (set-face-attribute 'flyspell-incorrect nil :box nil :underline '(:color "steelblue2" :style line)))
(use-package material-theme :defer t)
(use-package apropospriate-theme :defer t)
(use-package github-theme :defer t)
(use-package mustang-theme
  :defer t
  :config
  (set-face-attribute 'flycheck-error nil :box '(:line-width 2 :color "white" :style nil) :underline nil)
  (set-face-attribute 'flycheck-warning nil :box '(:line-width 2 :color "white" :style nil) :underline nil)
  (set-face-attribute 'flycheck-info nil :box '(:line-width 2 :color "white" :style nil) :underline nil)
  ;; mode-line faces customization
  (set-face-attribute 'mode-line nil :weight 'bold :background "#404040" :foreground "#eeeeec")
  (set-face-attribute 'mode-line-inactive nil :background "#404040" :foreground "#404040")
  (set-face-attribute 'mode-line-buffer-id nil :background "#404040" :foreground "#ff9800")
  ;; standard faces customization
  (set-face-attribute 'font-lock-warning-face nil :background "#202020" :foreground "#ff6523"))
(use-package tango-plus-theme
  :defer t
  :config
  (set-face-attribute 'flycheck-error nil :box t :underline nil)
  (set-face-attribute 'flycheck-warning nil :box t :underline nil)
  (set-face-attribute 'flycheck-info nil :box t :underline nil))
;;; must load the theme before specific package customization will take place
(load-theme 'nord t) ;; dark theme
;; (load-theme 'doom-molokai t) ;; dark theme
;; (load-theme 'dracula t) ;; dark theme
;; (load-theme 'mustang t) ;; dark default theme *****
;; (load-theme 'github t) ;; light theme *****

(use-package popwin
  :diminish popwin
  :config
  (progn
    (push '("*Occur*" :position bottom :height .3) popwin:special-display-config)
    (push '("*Org Select*" :position bottom :height .3) popwin:special-display-config)
    (push '("*compilation*" :position right :width 80 :noselect t) popwin:special-display-config)
    (popwin-mode 1)))

(use-package drag-stuff
  :diminish drag " "
  :config
  (progn
    (setq drag-stuff-except-modes '(org-mode))
    (drag-stuff-define-keys)
    (drag-stuff-global-mode 1)))

;;; org stuff
(use-package ob-http)

(use-package ob-mongo)

(use-package org
  :bind (("C-M-<return>" . org-insert-todo-subheading))
  :pin org
  :bind (("C-c c" . org-capture)
         :map org-mode-map
         ("C-x c s" . org-cut-subtree))
  :load-path "local-packages/"
  :config
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-use-property-inheritance t
        org-src-fontify-natively nil
        org-confirm-babel-evaluate nil
        org-tags-column -100
        org-startup-indented t
        org-return-follows-link t
        org-link-frame-setup '((file . find-file))
        org-support-shift-select 'always)
  (setq org-tag-persistent-alist '(("drill" . ?r)
                                   ("doing" . ?d)
                                   ("next" . ?n)
                                   ("today" . ?t)
                                   ("blocked" . ?b)
                                   ("jira" . ?j)
                                   ("trello" . ?l)))
  (require 'org-tempo)
  (require 'org-capture-functions)
  (add-hook 'org-capture-before-finalize-hook #'org-align-all-tags)
  (setq org-capture-templates
        '(("d" "Flash Card")
          ("ds" "Flash Card with Short Question and Single Answer"
           entry (function (lambda () (current-project-file+ask-headline "drill.org")))
           "** %^{Question} :drill:\n*** Answer\n    %?")
          ("dl" "Flash Card with Long Question and Single Answer"
           entry (function (lambda () (current-project-file+ask-headline "drill.org")))
           "** %^{Title} :drill:\n   %^{Question}\n*** Answer\n    %?")
          ("t" "Tasks")
          ("tt" "Task on current User Story"
           checkitem (function (lambda () (current-project-file+current-user-story ".project.org")))
           "- [ ] %?")
          ("tu" "Unrelated Task"
           checkitem (function (lambda () (current-project-file+headline ".project.org" "Unrelated")))
           "- [ ] %?")
          ("tr" "Refile Task"
           checkitem (function (lambda () (current-project-file+headline ".project.org" "Refile")))
           "- [ ] %?")
          ("r" "Code Review")
          ;; TODO: review of a region of code
          ;; TODO: add hash of the current head commit?
          ;; TODO: rename cc/org-capture-link-to-captured-line to cc/org-capture-link-to-current-line
          ;; TODO: add cc/org-capture-link-to-current-line-or-region
          ("rl" "Review of a line of code"
           item (function (lambda () (current-project-file+headline ".project.org" "Code Reviews")))
           "- %t %?\n  %(cc/org-capture-link-to-captured-line)")))
  ;; keep only the heading title without statistics cookies
  ;; ~something to be done [1/5]~ -> ~something to be done~
  (setq org-clock-heading-function
        (lambda ()
          (let ((heading (nth 4 (org-heading-components))))
            (if (string-match "^\\(?:\\(.*\\)\\s-+\\[[^]]+\\]\\)\\|\\(?:\\(.*\\)\\)$" heading)
                (or (match-string 1 heading) (match-string 2 heading))
              heading))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (http . t)
     (mongo . t)
     (sql . t)
     (shell . t))))

(use-package org-drill
  :ensure org-plus-contrib
  :after org
  :commands org-drill
  :init
  (require 'cl)                         ; avoid void-function copy-list error
  :config
  (add-to-list 'org-modules 'org-drill)
  (setq org-drill-add-random-noise-to-intervals-p t
        org-drill-hint-separator "||"
        org-drill-left-cloze-delimiter "<["
        org-drill-right-cloze-delimiter "]>"
        org-drill-learn-fraction 0.25))

;;; general utility
(use-package expand-region
  :bind (("M-]" . er/expand-region)
         ("M-[" . er/contract-region)))

(use-package multiple-cursors
  :bind (("C-c m n" . mc/mark-next-like-this)
         ("M-\\" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this-dwim)
	 ("C-c m l" . mc/edit-lines))
  :config
  (setq mc/always-run-for-all t))

(use-package visual-regexp
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))

(use-package hydra)

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell-check-startup-files nil)))

(use-package flyspell
  :diminish (flyspell-mode . " (S)")
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flycheck
  :commands flycheck-mode
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(mode-enabled save))
    (setq flycheck-idle-change-delay 3.14)
    (setq flycheck-highlighting-mode 'symbols)
    (setq flycheck-indication-mode nil)
    (setq flycheck-mode-line
          '(:eval
            (pcase flycheck-last-status-change
              (`not-checked " \uf00c")
              (`no-checker " \uf00c[-]")
              (`errored (propertize " \uf00c[!]" 'face '(:foreground "#ff0000")))
              (`interrupted " \uf00c[?]")
              (`suspicious " \uf00c[?]")
              (`running " \uf00c[?]")
              (`finished
               (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                      (n-errors (cdr (assq 'error error-counts)))
                      (n-warnings (cdr (assq 'warning error-counts))))
                 (if (or n-errors n-warnings)
                     (propertize
                      (format " \uf00c[%s/%s]" (or n-errors 0) (or n-warnings 0))
                      'face '(:foreground "Tomato"))
                   (propertize " \uf00c" 'face '(:foreground "#32cd32"))))))))
    (push '("*Flycheck errors*" :position bottom :height .4 :stick t) popwin:special-display-config)))

(use-package magit
  :bind (("C-c g s" . magit-status)
         ("H-s" . magit-status))
  :config
  (setq magit-section-visibility-indicator nil))

(use-package forge
  :after magit)

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine)
         ("H-t" . git-timemachine)))

(use-package git-gutter
  :diminish git-gutter-mode
  :bind (("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g r" . git-gutter:revert-hunk)
         ("C-c g a" . git-gutter:stage-hunk)
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
    (let ((git-gutter-default-fg "#d3d3d3"))
      (set-face-foreground 'git-gutter:added git-gutter-default-fg)
      (set-face-attribute 'git-gutter:added nil :height 80)
      (set-face-foreground 'git-gutter:deleted git-gutter-default-fg)
      (set-face-attribute 'git-gutter:deleted nil :height 80)
      (set-face-foreground 'git-gutter:modified git-gutter-default-fg)
      (set-face-attribute 'git-gutter:modified nil :height 80))))

;; ivy
(use-package smex)

(use-package ivy
  :demand t
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-S-s" . swiper-all)
         ("C-S-r" . swiper-all)
         ("C-M-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-h e l" . counsel-find-library)
         ("C-h e f" . find-function)
         ("C-h e v" . find-variable)
         ("C-h e m" . lisp-find-map)
         ("C-h e s" . counsel-info-lookup-symbol)
         ("C-h e u" . counsel-unicode-char)
         ("C-h e a v" . apropos-value)
         ("C-h e a l" . apropos-library)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         :map ivy-minibuffer-map
         ("C-s" . ivy-next-line)
         ("C-r" . ivy-previous-line))
  :init
  (unbind-key "C-h e")
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-wrap t
        ivy-fixed-height-minibuffer t)
  :config
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))
  (ivy-mode 1))

(use-package counsel
  :ensure counsel
  :after (ivy)
  :bind (:map counsel-find-file-map
              ("C-l" . counsel-up-directory)))

(use-package counsel-projectile
  :after (ivy projectile)
  :config
  (counsel-projectile-mode 1))

(use-package counsel-tramp
  :after (ivy)
  :bind (("C-x C-t" . counsel-tramp)
         ("C-x t" . counsel-tramp)))

(use-package ivy-hydra
  :after (ivy hydra))

;;; TOOD: ivy/counsel yasnippet
;;; TOOD: ivy/counsel erts
;;; TOOD: ivy/counsel colors

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-indexing-method 'alien
        projectile-enable-caching t)
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "**/elm-stuff")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories ".tmp")
  (add-to-list 'projectile-globally-ignored-directories ".work")
  (projectile-global-mode))

(use-package string-inflection
  :bind (("C-*" . string-inflection-all-cycle)))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-new-snippet-default nil)
  :config
  (yas-global-mode 1))

(use-package paredit
  :diminish (paredit-mode . " (P)")
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;;; elisp
(use-package eval-sexp-fu
  :diminish t
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode)
  :init
  (use-package highlight)
  (setq eval-sexp-fu-flash-face 'widget-field)
  (setq eval-sexp-fu-flash-error-face 'font-lock-warning-face)
  (setq eval-sexp-fu-flash-duration 0.3)
  :config
  ;; in theory it should be called already, in practice it isn't
  (esf-initialize))

;;; yaml
(use-package yaml-mode
  :mode "\\.yaml\\'" "\\.neon\\'")

;;; csv
(use-package csv-mode)

;;; gerkin
(use-package feature-mode
  :mode "\\.feature\\'")

;;; ruby
(use-package rspec-mode
  :init
  (setq rspec-spec-command "rspec")
  (setq rspec-key-command-prefix (kbd "C-c t")))

;;; elixir
(defun cc/alchemist-do-not-truncate-lines ()
  "Avoid truncate lines in alchemist buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match-p "^*alchemist" (buffer-name buffer))
      (with-current-buffer buffer
        (setq-local truncate-lines nil)))))

(use-package elixir-mode
  :init
  (defun cc/elixir--setup-mode ()
    (add-hook 'before-save-hook 'elixir-format nil t)
    t)
  (add-hook 'elixir-mode-hook #'cc/elixir--setup-mode))

(use-package flycheck-mix)

(use-package alchemist
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

;;; javascript --- TODO: tern, tide and completion?
(use-package rjsx-mode
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :hook (rjsx-mode . cc/jsx--setup)
  :init
  (defun cc/jsx--setup ()
    (when (executable-find "eslint")
      (flycheck-mode +1)
      (flycheck-select-checker 'javascript-eslint)))
  :config
  (setq standard-indent 2
        tab-width 1
        indent-tabs-mode nil
        js-indent-level 2
        js-switch-indent-offset t
        js2-basic-offset 2
        js2-jsx-mode 2
        js2-highlight-level 3
        js2-indent-level 2
        js2-indent-switch-body t
        js2-strict-semi-warning nil
        js2-missing-semi-one-line-override nil
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-strict-trailing-comma-warning nil
        sgml-basic-offset 2))

(use-package add-node-modules-path
  :after rjsx-mode
  :hook (rjsx-mode . add-node-modules-path))

(use-package prettier-js
  :diminish hindent-mode " ☰"
  :after (rjsx-mode add-node-modules-path)
  :hook (rjsx-mode . prettier-js-mode)
  :hook (json-mode . prettier-js-mode))

(use-package json-mode
  :mode (("\\.json\\'"  . json-mode)
         (".babelrc" . json-mode)
         (".prettierrc" . json-mode)
         (".eslintrc" . json-mode))
  :hook (json-mode . cc/json--setup)
  :init
  (defun cc/json--setup ()
    (when (executable-find "jsonlint")
      (flycheck-mode +1)
      (flycheck-select-checker 'json-jsonlint)))
  :config
  (setq js-indent-level 2))

;;; rust --- TODO: racer, clippy, flycheck-rust: navigation between errors doesn't work
(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :bind (("C-c <tab>" . rust-format-buffer))
  :config
  (progn
    (setq rust-format-on-save t)))

(use-package cargo
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
  :init
  (progn
    (add-hook 'rust-mode-hook #'flycheck-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
    ;; (add-hook 'rust-mode-hook #'yas-minor-mode)
;;; (add-hook 'rust-mode-hook #'flyspell-prog-mode)))

;;; haskell
;;; TODO: take a look at https://github.com/bitemyapp/dotfiles/blob/master/.emacs.d/haskell/hs-lint.el
(use-package haskell-mode
  :config
  (setq haskell-process-type 'ghci)
  ;; (setq haskell-process-path-stack "/usr/local/bin/stack")
  ;; (setq haskell-process-args-stack-ghci "ghci")
  (setq haskell-process-path-ghci "/usr/local/bin/stack")
  (setq haskell-process-args-ghci '("ghci"))
  (setq inferior-haskell-root-dir "/home/coder/tmp"))

(use-package intero
  :hook (haskell-mode . intero-mode)
  :config (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(when (executable-find "hindent")
  (use-package hindent
    :diminish hindent-mode " ↹"
    :hook (haskell-mode . hindent-mode)
    :config
    (setq hindent-reformat-buffer-on-save t)))

;;; elm
(use-package elm-mode
  :mode ("\\.elm'" . elm-mode)
  :config
  (progn
    (push '("*elm-make*" :position right :width 80 :noselect t) popwin:special-display-config)
    (setq elm-indent-offset 4)
    (setq elm-format-on-save t)))

(use-package flycheck-elm
  :init
  (defun cc/setup-elm-mode ()
    (flycheck-mode)
    (setq flycheck-check-syntax-automatically '(mode-enabled save))
    (flycheck-elm-setup))
  (add-hook 'elm-mode-hook #'cc/setup-elm-mode))

;;; clojure
(use-package cider)

;;; toml
(use-package toml-mode
  :mode ("\\.toml$" . toml-mode))

;;; dockerfile
(use-package dockerfile-mode)
(use-package docker-tramp)
(use-package docker)
  ;; :ensure t
  ;; :bind ("C-c d" . docker))

;;; markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc --from markdown_github -t html5 -s"))

;;; php
(use-package php-mode
  :mode "\\.php\\'"
  :init
  (defun cc/setup-php-mode ()
    (c-set-offset 'case-label '+)
    (modify-syntax-entry ?$ "w" php-mode-syntax-table)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(mode-enabled save)))
  (add-hook 'php-mode-hook #'cc/setup-php-mode))

;;; TODO psysh
;;; (use-package psysh)
;;; (use-package phpactor)

(use-package phpunit
  :after php-mode)

(use-package flycheck-phpstan
  :after php-mode
  :init
  (defun cc/php-setup-phpstan ()
    (let ((phpstan (executable-find "phpstan")))
      (when (not phpstan)
        (user-error "Unable to find phpstan executable, maybe it is not installed?"))
      (setq-local phpstan-level 'max)
      (setq-local phpstan-executable phpstan)
      (flycheck-select-checker 'phpstan)))
  (add-hook 'php-mode-hook #'cc/php-setup-phpstan))

(use-package php-functions
  :ensure nil
  :after php-mode
  :load-path "local-packages/"
  :bind (:map php-mode-map
              ("C-c i" . 'php-use-at-point)
              ("C-c s" . 'php-normalize-use-region)))

;;; chunkly
(use-package chunkly-mode
  :ensure nil
  :commands (chunkly-mode)
  :load-path "local-packages/"
  :mode ("\\.chunkly/[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.log\\'" . chunkly-mode))

;;; functions
(defun cc/load-local-machine-configuration (&optional machine)
  "Load configuration of the current machine or for MACHINE.

The name of the loaded file is `<MACHINE-NAME>-configuration.el`
and must be placed and can be found under
`~/.emacs.d/local-packages` directory"
  (interactive)
  (let* ((machine-name (or machine (string-trim (shell-command-to-string "hostname -f"))))
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

(defun cc/duplicate-line-or-region-above (arg)
  "Duplicates the current line or region above the original."
  (interactive "p")
  (cc/duplicate-line-or-region (- arg)))

(defalias 'cc/duplicate-line-or-region-below #'cc/duplicate-line-or-region)

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
      (setq env-export (split-string line "[ =]+" nil))
      (setenv (car env-export) (cadr env-export)))))

(defun cc/copy-character-from-above ()
  "Copy one character from previous non blank line starting above point."
  (interactive)
  (cc/copy-character-from-around -1))

(defun cc/copy-character-from-below ()
  "Copy one character from previous non blank line starting above point."
  (interactive)
  (cc/copy-character-from-around 1))

(defun cc/kill-current-file-name ()
  "Put current buffer file name with extension in kill ring."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (kill-new (file-name-without-directory file-name)))))

(defun cc/kill-current-file-path ()
  "Put current buffer file path in kill ring."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (kill-new file-name))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06fs" (float-time (time-since time)))))

(defun buffer-base-name ()
  "Current buffer file name without directory and suffix."
  (->>
   (buffer-file-name)
   (file-name-without-extension)
   (file-name-without-directory)))

;;; I can get crazy for inconsistencies so...
(defalias 'nullp 'null)
(defalias 'atomp 'atom)
(defalias 'file-name-without-extension 'file-name-sans-extension)
(defalias 'file-name-without-directory 'file-name-nondirectory)
(defalias 'buffer-basename 'buffer-base-name)
(defalias 'buffer-filename 'buffer-file-name)

;;; global bindings
(bind-key "H-p" #'cc/open-line-above)
(bind-key "H-n" #'cc/open-line-below)
(bind-key "H-<return>" #'cc/open-line-here)
(bind-key "M-SPC" #'rectangle-mark-mode)
(bind-key "H-u" #'cc/copy-character-from-above)
(bind-key "H-d" #'cc/copy-character-from-below)
(bind-key "C-a" #'cc/smarter-move-beginning-of-line)
(bind-key "C-c l" #'org-store-link)
(bind-key "C-^" #'cc/join-with-next-line)
(bind-key "C-;" #'cc/comment-or-uncomment-line-or-region)
(bind-key "C-x e" #'cc/eval-and-replace)
(bind-key "M-p" #'cc/duplicate-line-or-region-above)
(bind-key "M-n" #'cc/duplicate-line-or-region-below)
(bind-key "M-o" #'other-window)
(bind-key "C-c D" #'cc/delete-current-buffer-and-file)
(bind-key "C-c R" #'cc/rename-current-buffer-and-file)
(bind-key "C-x &" #'kmacro-call-macro)
(bind-key "C-c k f n" #'cc/kill-current-file-name)
(bind-key "C-c k f p" #'cc/kill-current-file-path)
(bind-key "C-c k l" #'kill-whole-line)

(bind-key "C-c e b" #'eval-buffer emacs-lisp-mode-map)
(bind-key "C-c e d" #'eval-defun emacs-lisp-mode-map)
(bind-key "C-c e f" #'emacs-lisp-byte-compile-and-load emacs-lisp-mode-map)
(bind-key "C-c e r" #'eval-region emacs-lisp-mode-map)
(bind-key "C-c e =" #'cc/eval-and-replace)
(bind-key "C-c e t" #'ert emacs-lisp-mode-map)
(bind-key "C-c e e" #'toggle-debug-on-error)
(bind-key "C-c e s" #'scratch)
(bind-key "C-c e m" #'view-echo-area-messages)

;;; global hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'compilation-filter-hook 'cc/colorize-compilation)

;;; global configuration
(setq user-full-name "Gabriele Lana")
(setq user-mail-address "gabriele.lana@gmail.com")
(setq tramp-terminal-type "dumb")
(setq tramp-default-method "ssh")
(setq tramp-remote-shell "/bin/sh")
;;; always indent after yank
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (not (member major-mode '()))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))
;;; use UTF-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;; ask `y or n' rather than `yes or no'
(defalias 'yes-or-no-p 'y-or-n-p)
;;; don't backup files
(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'create-lockfiles nil)
;;; don't blink the cursor
(customize-set-variable 'blink-cursor-mode nil)
;;; when scroll to the bottom/top then place the cursor to the very last/first line
(customize-set-variable 'scroll-error-top-bottom t)
;;; invoke commands that use minibuffers even while the minibuffer window is active
(setq enable-recursive-minibuffers t)
;;; do not wait to have fully rendered the buffer before accepting inputs
(setq redisplay-dont-pause nil)
;;; highlight current line
(global-hl-line-mode +1)
;;; replace region when you type something and a region is active
(delete-selection-mode)
;;; diplay line and column number in modeline
(line-number-mode t)
(column-number-mode t)
;;; more room in the macro's kill ring
(setq kmacro-ring-max 100)
;;; better performance sacrificing right-to-left languages
(setq-default bidi-display-reordering nil)
;;; alleviate bug of emacs-26 see https://github.com/emacs-helm/helm/issues/1976
(when (eq emacs-major-version 26)
  (setq x-wait-for-event-timeout nil))
;;; dired
(setq dired-dwim-target t)

;;; shell-script-mode default configurations
(setq sh-basic-offset 2)

;;; local configuration
(cc/load-local-machine-configuration)

;;; appearance
(setq visible-bell nil)
(setq inhibit-splash-screen t)
(set-frame-font "PragmataPro Mono 16")

;;; enable some "dangerous" commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; diminish things
(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)
