;;; -*- lexical-binding: t -*-
(setq-default lexical-binding t)

;;; TODO: make "starred buffers" to be ranked low in ivy

;;; handle garbage collection
(defun cc/defer-garbage-collection ()
  (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
        gc-cons-percentage 0.6))

(defun cc/restore-garbage-collection ()
  (setq gc-cons-threshold 134217728     ; 128 mega bytes
        gc-cons-percentage 0.1))

;;; disable garbage collection at startup
(cc/defer-garbage-collection)
(add-hook 'emacs-startup-hook #'cc/restore-garbage-collection)

;;; disable garbage collection in minibuffer
(add-hook 'minibuffer-setup-hook #'cc/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook (lambda () (run-at-time 1 nil #'cc/restore-garbage-collection)))

;;; bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; to update all packages run `straight-pull-package'
;;; download and set up use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package diminish :demand t)
(use-package bind-key :demand t)
(use-package s :demand t)
(use-package f :demand t)
(use-package uuidgen :demand t)
(use-package dash :demand t)
(use-package request :demand t)

;;; put custom configurations aside
(setq custom-file "~/.emacs.d/custom.el")
(when (not (file-exists-p custom-file))
  (write-region "" nil custom-file))
(load custom-file)

;;; start the server if not already started
(load "server")
(unless (server-running-p) (server-start))

;;; default configuration
(use-package better-defaults)
(use-package scratch)

;;; themes
(use-package all-the-icons
  :straight t
  :config
  (setq all-the-icons-color-icons nil))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(use-package apropospriate-theme)
(use-package github-theme)
(use-package material-theme)
(use-package monokai-theme)
(use-package subatomic-theme
  :defer t
  :init
  (setq subatomic-more-visible-comment-delimiters t)
  ;; (set-face-attribute 'hl-line nil :background "#4b5275")
  )
(use-package doom-themes)
(use-package nord-theme
  :defer t
  :init
  (setq nord-region-highlight "LightSteelBlue")
  (setq nord-comment-brightness 20))
(use-package mustang-theme
  :defer t
  :config
  (set-face-attribute 'font-lock-warning-face nil
                      :background "#202020"
                      :foreground "#ff6523"))

;;; light themes
;; (load-theme 'dichromacy t)
;; (load-theme 'github t)

;;; dark themes
(load-theme 'subatomic t)
;; (load-theme 'doom-molokai)
;; (load-theme 'doom-dracula)
;; (load-theme 'doom-nord)
;; (load-theme 'nord t)

;;; modeline
(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode)
         (after-load-theme . cc/doom-modeline-setup-theme)
         (doom-modeline-mode . cc/doom-modeline-setup-custom-modeline))
  :init
  (defun cc/doom-modeline-setup-theme ()
    (set-face-attribute 'doom-modeline-bar nil
                        :background (face-attribute 'cursor :background)
                        :inherit 'unspecified)
    (set-face-attribute 'doom-modeline-project-dir nil
                        :inherit 'doom-modeline-buffer-major-mode))
  (defun cc/doom-modeline-setup-custom-modeline ()
    (doom-modeline-set-modeline 'cc 'default))
  :config
  (setq inhibit-compacting-font-caches t
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-irc nil
        doom-modeline-major-mode-color-icon nil
        doom-modeline-modal-icon nil
        doom-modeline-mu4e nil
        doom-modeline-project-detection 'projectile)
  (doom-modeline-def-segment pad
    "Give more space"
    (doom-modeline-spc))
  (doom-modeline-def-modeline 'cc
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info  grip debug lsp minor-modes indent-info buffer-encoding major-mode process checker vcs pad))
  (cc/doom-modeline-setup-theme))

;;; universal minor modes
(use-package highlight-indent-guides
  :hook ((yaml-mode . highlight-indent-guides-mode)
         (json-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))

(use-package direnv
  :straight t
  :config
  (direnv-mode))

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

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

(use-package expand-region
  :bind (("M-]" . er/expand-region)
         ("M-[" . er/contract-region)))

(use-package multiple-cursors
  :bind (("M-\\" . mc/mark-next-like-this)
         :map mc/keymap
         ("C-'" . mc-hide-unmatched-lines-mode))
  :config
  (setq mc/always-run-for-all nil))

(use-package visual-regexp
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))

(use-package hydra
  :straight t
  ;; :after multiple-cursors
  :config
  (global-set-key
   (kbd "C-c m")
   (defhydra hydra-multiple-cursors (:hint nil)
     "
 ^multiple-cursors
 ^Up^             ^Down^           ^Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_i_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_b_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit"
     ("l" mc/edit-lines :exit t)
     ("a" mc/mark-all-like-this :exit t)
     ("n" mc/mark-next-like-this)
     ("N" mc/skip-to-next-like-this)
     ("M-n" mc/unmark-next-like-this)
     ("p" mc/mark-previous-like-this)
     ("P" mc/skip-to-previous-like-this)
     ("M-p" mc/unmark-previous-like-this)
     ("s" mc/mark-all-in-region-regexp :exit t)
     ("i" mc/insert-numbers :exit t)
     ("b" mc/insert-letters :exit t)
     ("<mouse-1>" ignore)
     ("<down-mouse-1>" ignore)
     ("<drag-mouse-1>" ignore)
     ("q" nil))))

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell-check-startup-files nil)))

;;; org mode
(use-package ob-http)

(use-package ob-mongo)

(use-package org
  :straight org-plus-contrib
  :bind (("C-c c" . org-capture)
         ("C-M-<return>" . org-insert-todo-subheading)
         :map org-mode-map
         ("C-x c s" . org-cut-subtree)
         ("C-c C-x C-i" . org-clock-in))
  :load-path "local-packages/"
  :config
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-use-property-inheritance t
        org-src-fontify-natively nil
        org-confirm-babel-evaluate nil
        org-catch-invisible-edits 'error
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
  (require 'org-functions)
  (add-hook 'org-mode-hook #'cc/org-mode-buffer-setup)
  (add-hook 'org-babel-after-execute-hook #'cc/org-mode-buffer-force-uppercase-keywords)
  (require 'org-capture-functions)
  (add-hook 'org-capture-before-finalize-hook #'org-align-all-tags)
  (setq org-capture-templates
        '(("d" "Flash Card")
          ("ds" "Flash Card with Short Question and Single Answer"
           entry (function (lambda () (current-project-file+ask-headline "drill.org")))
           "** %^{Question} :drill:\n*** Answer\n%?\n")
          ("dl" "Flash Card with Long Question and Single Answer"
           entry (function (lambda () (current-project-file+ask-headline "drill.org")))
           "** %^{Title} :drill:\n{LONG_QUESTION}\n*** Answer\n%?\n")
          ("de" "Flash Card with Sentence"
           entry (function (lambda () (current-project-file+ask-headline "drill.org")))
           "** Sentence :drill:\n%?\n")
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
           "- %t %?\n%(cc/org-capture-link-to-captured-line)")))
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
  :after org
  :commands org-drill
  :config
  (add-to-list 'org-modules 'org-drill)
  (setq org-drill-add-random-noise-to-intervals-p t
        org-drill-hint-separator "||"
        org-drill-left-cloze-delimiter "<["
        org-drill-right-cloze-delimiter "]>"
        org-drill-learn-fraction 0.25))

(use-package org-tree-slide
  :after org
  :bind (("H-n" . org-tree-slide-move-next-tree)
         ("H-p" . org-tree-slide-move-previous-tree)
         :map org-mode-map
         ("C-c s" . org-tree-slide-mode))
  :config
  (org-tree-slide-simple-profile))

;;; check
(use-package flyspell
  :diminish (flyspell-mode . " (S)")
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (after-load-theme . cc/flycheck-setup-theme))
  :init
  (defun cc/flyspell-setup-theme ()
    (set-face-attribute 'flyspell-incorrect nil
                        :underline '(:color "red1" :style line))
    (set-face-attribute 'flyspell-duplicate nil
                        :underline '(:color "orange" :style line)))
  :config
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-," flyspell-mode-map)
  (unbind-key "C-;" flyspell-mode-map)
  (cc/flyspell-setup-theme))

(use-package flycheck
  :commands flycheck-mode
  :hook (after-load-theme . cc/flycheck-setup-theme)
  :init
  (defun cc/flycheck-setup-theme ()
    "Setup flycheck in accordance to the current theme."
    (let ((default-foreground (face-attribute 'default :foreground)))
      (set-face-attribute 'flycheck-error nil
                          :box `(:line-width 1 :color ,default-foreground :style nil)
                          :underline nil)
      (set-face-attribute 'flycheck-warning nil
                          :box `(:line-width 1 :color ,default-foreground :style nil)
                          :underline nil)
      (set-face-attribute 'flycheck-info nil
                          :box `(:line-width 1 :color ,default-foreground :style nil)
                          :underline nil)))
  :config
    ;; (push '("*Flycheck errors*" :position bottom :height .4 :stick t) popwin:special-display-config)
    (setq flycheck-check-syntax-automatically '(mode-enabled save)
          flycheck-idle-change-delay 3.14
          flycheck-highlighting-mode 'symbols
          flycheck-indication-mode nil)
    (cc/flycheck-setup-theme))

;;; version control
(use-package magit
  :bind (("C-c g s" . magit-status)
         ("C-c g f" . cc/git-add-with-force-current-buffer)
         ("H-s" . magit-status))
  :hook (git-commit-setup . cc/insert-commit-message)
  :custom
  (magit-section-visibility-indicator nil)
  (transient-display-buffer-action '(display-buffer-below-selected))
  :config
  (defvar cc/first-commit-messages
    '("This is where it all begins..."
      "Ready, Set, Go!"
      "A long time ago in a galaxy far, far away..."
      "Here be dragons"
      "In the beginning there was darkness"
      "Reinventing the wheel. Again"
      "Batman! (this commit has no parents)"
      "First blood"
      "Don’t ask"
      "Look behind you!!!"
      "Ship It!"
      "A beginning is a very delicate time (Frank Herbert)")
    "List of initial commit messages for a git repository.")
  (defun cc/insert-commit-message ()
    (let* ((current-commit (magit-git-string "rev-parse" "HEAD"))
           (current-commit-parents (magit-commit-parents current-commit)))
      (when (null current-commit-parents)
        (insert (cc/pick-random cc/first-commit-messages))
        (save-buffer))))
  (defun cc/git-add-with-force-current-buffer ()
    "Adds (with force) the file from the current buffer."
    (interactive)
    (shell-command (concat "git add -f " (shell-quote-argument buffer-file-name)))))

(use-package forge
  :after magit)

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine)
         ("H-t" . git-timemachine)))

(use-package git-gutter
  :bind (("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g r" . git-gutter:revert-hunk)
         ("C-c g a" . git-gutter:stage-hunk)
         ("C-c g u" . git-gutter:update-all-windows))
  :hook (after-load-theme . cc/git-gutter-setup-theme)
  :init
  (defun cc/git-gutter-setup-theme ()
    "Configure git gutter in accordance to the current theme."
    (let ((default-fg (face-attribute 'default :foreground))
          (default-bg (face-attribute 'default :background)))
      (set-face-attribute 'git-gutter:added nil
                          :foreground default-fg
                          :background default-bg
                          :height 0.8)
      (set-face-attribute 'git-gutter:deleted nil
                          :foreground default-fg
                          :background default-bg
                          :height 0.8)
      (set-face-attribute 'git-gutter:modified nil
                          :foreground default-fg
                          :background default-bg
                          :height 0.8)))
  :config
  (custom-set-variables
   '(git-gutter:window-width 2)
   '(git-gutter:added-sign "\uf067")
   '(git-gutter:deleted-sign "\uf068")
   '(git-gutter:modified-sign "\uf054")
   '(git-gutter:hide-gutter nil))
  (cc/git-gutter-setup-theme))

;;; ivy
(use-package smex)

(use-package ivy
  :demand t
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-S-s" . swiper-all)
         ("C-S-r" . swiper-all)
         ("C-`" . ivy-resume)
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
  ;; :ensure counsel
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

;;; TODO: ivy/counsel yasnippet
;;; TODO: ivy/counsel erts
;;; TODO: ivy/counsel colors

(use-package projectile
  :init
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t)
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "**/elm-stuff")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories ".tmp")
  (add-to-list 'projectile-globally-ignored-directories ".work")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode))

(use-package company
  :bind (:map company-mode-map
              ("C-<tab>" . company-complete))
  :custom
  (company-show-numbers t)
  (company-idle-delay 0.1)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1))

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

(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

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

;;; erlang
(use-package erlang)

;;; elixir
(use-package elixir-mode
  :hook ((elixir-mode . cc/elixir--setup)
         (lsp-after-initialize . cc/elixir--lsp-setup))
  :init
  (defun cc/elixir--lsp-setup ()
    (let ((lsp-elixir--config-options (make-hash-table)))
      (puthash "dialyzerEnabled" :json-false lsp-elixir--config-options)
      (lsp--set-configuration `(:elixirLS ,lsp-elixir--config-options))))

  (defun cc/elixir--setup ()
    (lsp)
    (lsp-ui-mode)
    (company-mode)    (flycheck-mode)
    (setq-local flycheck-checker 'lsp)
    (flycheck-add-next-checker 'lsp 'elixir-credo)
    ;; (flycheck-add-next-checker 'lsp-ui 'elixir-credo)
    (setq-local flycheck-elixir-credo-strict t)
    (setq-local lsp-ui-doc-enable t)
    (setq-local lsp-ui-doc-use-childframe t)
    (setq-local lsp-log-io t)
   (add-hook 'before-save-hook 'elixir-format nil t)
    ))

(use-package mix
  :straight t
  :bind (:map mix-minor-mode-map
              ("C-c a t t" . mix-test)
              ("C-c a m t t" . mix-test)
              ("C-c a t b" . mix-test-current-buffer)
              ("C-c a m t b" . mix-test-current-buffer)
              ("C-c a t f" . mix-test-curret-test)
              ("C-c a m t f" . mix-test-current-test))
  :hook (elixir-mode . mix-minor-mode))

;; (use-package flycheck-mix
;;   :straight t
;;   :hook ((elixir-mode . flycheck-mode)
;;          (elixir-mode . flycheck-mix-setup)))

;;; TODO
;; (use-package elixir-mix
;;   :straight (:host github :repo "gabrielelana/emacs-elixir-mix"))

;;; javascript
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
  :diminish prettier-js-mode " ☰"
  :hook ((rjsx-mode json-mode css-mode yaml-mode markdown-mode js-mode typescript-mode) . prettier-js-mode))

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

;;; lsp
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("M-RET" . lsp-execute-code-action))
  :hook (lsp-mode . cc/lsp--setup)
  :init
  (setq lsp-keymap-prefix "H-l")
  (defun cc/lsp--setup ()
    (make-local-variable 'read-process-output-max)
    (setq read-process-output-max 1048576))
  :config
  (setq lsp-keymap-prefix "H-l"
        lsp-auto-guess-root t
        lsp-prefer-capf t
        lsp-idle-delay 0
        lsp-enable-file-watchers nil
        lsp-print-io nil
        lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :bind (:map lsp-mode-map
              ("H-l h d" . lsp-ui-doc-show)
              ("H-l h D" . lsp-ui-doc-hide)
              ("H-d" . lsp-describe-thing-at-point))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-doc-max-width 80)
  ;; lsp-ui-doc-delay Number of seconds before showing the doc
  ;; (lsp-ui-doc-delay 0)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  ;; lsp-ui-doc-position Where to display the doc
  ;; lsp-ui-doc-enable enable lsp-ui-doc
  ;; (setq lsp-ui-doc-enable t
  ;;       lsp-ui-doc-position 'top
  ;;       lsp-ui-doc-delay 3)
  )

(use-package lsp-ivy
  :straight t
  :bind (("H-l y" . lsp-ivy-workspace-symbol))
  :commands lsp-ivy-workspace-symbol)

;;; rust
(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :bind (("C-c <tab>" . rust-format-buffer))
  :hook (rust-mode . cc/rust--setup)
  :bind (:map rust-mode-map
              ("TAB" . company-indent-or-complete-common))
  :init
  (defun cc/rust--setup ()
    (use-package flycheck-rust)
    (lsp)
    (lsp-ui-mode)
    (flycheck-mode)
    (company-mode)
    (flymake-mode -1)
    (flycheck-rust-setup))
  :config
  (setq rust-format-on-save t)
  (setq lsp-rust-full-docs t)
  (setq lsp-rust-build-on-save t))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode)

;;; haskell
(use-package haskell-mode
  :hook (haskell-mode . cc/haskell--setup)
  :init
  (defun cc/haskell--setup ()
    (lsp)
    (lsp-ui-mode)
    (flycheck-mode)
    (flymake-mode -1)
    (company-mode)
    (setq-local lsp-ui-doc-enable nil)
    (setq-local lsp-ui-doc-use-childframe t)
    (setq-local lsp-flycheck-live-reporting nil)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))
  :config
  (setq haskell-prompt-regexp "^\\([>|] \\)+")
  (setq haskell-process-type 'ghci)
  (setq haskell-process-path-ghci (executable-find "stack"))
  (setq haskell-process-args-ghci '("ghci"))
  (setq inferior-haskell-root-dir "/tmp"))

(use-package lsp-haskell
  :config
  (setq lsp-haskell-process-path-hie (executable-find "ghcide"))
  (setq lsp-haskell-process-args-hie '())
  (setq lsp-log-io nil))

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

;;; graphql
(use-package graphql-mode
  :straight t)

;;; dockerfile
(use-package dockerfile-mode)
(use-package docker-tramp)
(use-package docker)

;;; markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom-face (markdown-code-face ((t (:inherit nil))))
  :init
  (setq markdown-fontify-code-blocks-natively nil)
  (setq markdown-command "pandoc --from markdown_github -t html5 -s"))

;;; php
(use-package php-mode
  :mode "\\.php\\'"
  :init
  (defun cc/php-setup ()
    (c-set-offset 'case-label '+)
    (modify-syntax-entry ?$ "w" php-mode-syntax-table)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(mode-enabled save)))
  (add-hook 'php-mode-hook #'cc/php-setup))

(use-package phpunit
  :after php-mode)

(use-package flycheck-phpstan
  :after php-mode
  :init
  (defun cc/flycheck-phpstan--setup()
    (when (phpstan-get-executable)
      (when (not (phpstan-get-configuration-file))
        (user-error "Unable to find ./phpstan.neon, without it flycheck-phpstan doesn't work"))
      (setq-local phpstan-level 'max)
      (flycheck-select-checker 'phpstan)))
  (add-hook 'php-mode-hook #'cc/flycheck-phpstan--setup))

;;; TODO: restore
;; (use-package php-functions
;;   ;; :ensure nil
;;   :after php-mode
;;   :load-path "local-packages/"
;;   :bind (:map php-mode-map
;;               ("C-c r i" . 'php-import-classname-at-point)
;;               ("C-c r s" . 'php-normalize-use-region)))

;;; TODO: restore
;; ;; chunkly
;; (use-package chunkly-mode
;;   ;; :ensure nil
;;   :commands (chunkly-mode)
;;   :load-path "local-packages/"
;;   :mode ("\\.chunkly/[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.log\\'" . chunkly-mode))

;;; functions
(defun cc/pick-random (l)
  "Pick a random element from a list L."
  (nth (random (length l)) l))

(defun cc/recompile-packages ()
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

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

(defun cc/remove-flyspell-errors (begin end _)
  "Removes all flyspell overlays on region."
  (flyspell-delete-region-overlays begin end))

(advice-add #'uncomment-region :after #'cc/remove-flyspell-errors)

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

;;; to make uuidgen functions work, consider using straight-el
(defun math-fixnum (a)
  (if (consp a)
      (if (cdr a)
	  (if (eq (car a) 'bigneg)
	      (- (math-fixnum-big (cdr a)))
	    (math-fixnum-big (cdr a)))
	0)
    a))

(defun math-fixnum-big (a)
  (if (cdr a)
      (+ (car a) (* (math-fixnum-big (cdr a)) 1000))
    (car a)))

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
(bind-key "C-M-y" #'yank-pop)
(bind-key "C-M-s" #'isearch-forward)
(bind-key "C-M-S-s" #'isearch-forward-regexp)
(bind-key "C-M-r" #'isearch-backward)
(bind-key "C-M-S-r" #'isearch-backward-regexp)
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

;;; keybindings to insert symbols
(global-set-key (kbd "C-c i l") "λ")
(global-set-key (kbd "C-c i a") "∧")
(global-set-key (kbd "C-c i o") "∨")
(global-set-key (kbd "C-c i >") "→")
(global-set-key (kbd "C-c i <") "←")
(global-set-key (kbd "C-c i =") "≡")
(global-set-key (kbd "C-c i b") "⊥")
(global-set-key (kbd "C-c i f") "∀")
(global-set-key (kbd "C-c i t") "⊤")

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
;;; should improve re-display issues?
(setq recenter-redisplay nil)
;;; highlight current line
(global-hl-line-mode +1)
;;; highlight current line
(global-git-gutter-mode +1)
;;; replace region when you type something and a region is active
(delete-selection-mode)
;;; diplay line and column number in modeline
(line-number-mode t)
(column-number-mode t)
;;; more room in the macro's kill ring
(setq kmacro-ring-max 100)
;;; better performance sacrificing right-to-left languages
(setq-default bidi-display-reordering nil)
;;; dired
(setq dired-dwim-target t)
;;; recently added to Emacs 27, improves performance
(setq read-process-output-max (* 1024 1024))
;;; underlining
(setq underline-minimum-offset 2)
(setq x-underline-at-descent-line t)

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
