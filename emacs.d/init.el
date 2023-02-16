;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Bootstrap Emacs configuration, install and configure a bunch of packages
;;;
;;; Code:

;; Compile Emacs with the following commands
;; $ cd ~/src/emacs
;; $ git pull
;; $ ./configure --prefix=/home/coder/opt/emacs-native --bindir=/home/coder/opt/emacs-native/bin --with-modules --with-cairo --with-imagemagick --with-x=yes --with-x-toolkit=gtk3 --with-xwidgets --without-dbus --with-native-compilation --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm --with-json --with-mailutils
;; $ make && make install # without parallel compilation aka -jN option

;; Compile Emacs with --with-pgtk
;; $ ./configure --prefix=/home/coder/opt/emacs-native --bindir=/home/coder/opt/emacs-native/bin CFLAGS="-O2 -pipe -mtune=native -march=native -fomit-frame-pointer" --without-dbus --without-imagemagick --without-gsettings --with-modules --with-cairo --with-xwidgets --with-native-compilation --with-jpeg --with-png --with-rsvg --with-tiff --with-xpm --with-json --with-wide-int --with-xml2 --with-harfbuzz --with-mailutils --with-pgtk
;; $ make -j`nproc` && make install

;; Start Emacs with the following commands
;; $ ~/opt/emacs-native/bin/emacs -Q --load=~/code/emacs-playground/init.el &

;; Inspired from
;; - https://github.com/ianyepan/.wsl-emacs.d/blob/master/init.el
;; - https://github.com/purcell/emacs.d
;; - ...

;;; The following two lines can be removed when done and when this
;;; file will be moved in the canonical place ~/.emacs.d/init.el
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(add-to-list 'load-path (concat user-emacs-directory "local-packages"))

;;; Below your experimental configuration

(setq package-enable-at-startup nil)

;;; Native compilation
(setq native-comp-speed 2
      comp-speed 2)
(setq native-comp-async-report-warnings-errors nil
      comp-async-report-warnings-errors nil)
(setq native-comp-async-query-on-exit t
      comp-async-query-on-exit t)

;;; Prevent native compilation of .dir-locals.el files.
(let ((deny-list '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)")))
  (if (boundp 'native-comp-deferred-compilation-deny-list)
      (setq native-comp-deferred-compilation-deny-list deny-list)
    (setq comp-deferred-compilation-deny-list deny-list)))

;;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 64 1024 1024))
      (startup-gc-cons-threshold (* 256 1024 1024))
      (normal-gc-cons-percentage 0.1)
      (startup-gc-cons-percentage 0.6))
  (setq gc-cons-threshold startup-gc-cons-threshold)
  (setq gc-cons-percentage startup-gc-cons-percentage)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold
                    gc-cons-percentage normal-gc-cons-percentage))))

;; Appearance
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode -1)
(electric-indent-mode +1)
(setq visible-bell nil)
(setq inhibit-splash-screen t)
(setq frame-resize-pixelwise t)
(setq split-width-threshold 100)
;; (setq cursor-type '(bar . 3))
;; (setq cursor-type 'box)

(set-frame-font "PragmataPro Mono 14")

;;; Bootstrap straight
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

;;; Integrate use-package with straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; Configuration of built-in functionalities
(require 'better-defaults-before)
(require 'personal-functions)

;;; TODO: tentative, if ok then move to better-defaults-before
;;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Margins.html
(setq-default left-margin-width 0)
(setq-default right-margin-width 0)

;;; General utilities and libraries
(use-package blackout)
(use-package bind-key)
(use-package s)
(use-package f)
(use-package ht)
(use-package uuidgen)
(use-package dash)
(use-package request)

(let ((local-repository (expand-file-name "~/code/retro.el")))
  (if (file-directory-p local-repository)
      (use-package retro
        :straight `(retro :local-repo ,local-repository
                          :files ("*.el")))
    (use-package retro
      :straight (retro :type git
                       :host github
                       :repo "gabrielelana/retro.el"
                       :files ("*.el")))))

(let ((local-repository (expand-file-name "~/code/emacs-nes")))
  (if (file-directory-p local-repository)
    (use-package emacs-nes
      :straight `(emacs-nes
                  :type git
                  :local-repo ,local-repository
                  :build (compile native-compile)
                  :files ("*.el")))
    (use-package emacs-nes
      :straight (emacs-nes :type git
                           :host github
                           :repo "gabrielelana/emacs-nes"
                           :build (compile native-compile)
                           :files ("*.el")))))

;;; Theme & Appearance
(use-package all-the-icons)
(use-package smooth-scrolling)

;;; https://github.com/aaronjensen/night-owl-emacs
(use-package night-owl-theme)
(use-package kaolin-themes)
(use-package apropospriate-theme)
(use-package dracula-theme)
(use-package nord-theme)
(use-package subatomic-theme
  :config
  (custom-set-faces
   '(org-block ((t (:inherit font-lock-comment-delimiter-face))))
   '(org-code ((t (:inherit font-lock-comment-delimiter-face))))
   '(org-verbatim ((t (:inherit font-lock-comment-delimiter-face))))
   '(org-document-title ((t (:inherit org-document-info))))
   '(term-color-black ((t (:foreground "#2e3043" :background "#2e3043"))))
   '(term-color-red ((t (:foreground "#ea8673" :background "#ea8673"))))
   '(term-color-green ((t (:foreground "#a9dc69" :background "#a9dc69"))))
   '(term-color-yellow ((t (:foreground "#ffd700" :background "#ffd700"))))
   '(term-color-blue ((t (:foreground "#8aa6bc" :background "#8aa6bc"))))
   '(term-color-magenta ((t (:foreground "#feccd4" :background "#feccd4"))))
   '(term-color-cyan ((t (:foreground "#9c71a5" :background "#9c71a5"))))
   '(term-color-white ((t (:foreground "#e5e5e5" :background "#e5e5e5")))))
  :custom
  subatomic-more-visible-comment-delimiters t)

(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode)
         (after-load-theme . cc/doom-modeline-setup-theme)
         (doom-modeline-mode . cc/doom-modeline-setup-custom-modeline))
  :preface
  (defun cc/doom-modeline-setup-theme ()
    (set-face-attribute 'doom-modeline-bar nil
                        :background (face-attribute 'cursor :background)
                        :inherit 'unspecified)
    (set-face-attribute 'doom-modeline-project-dir nil
                        :inherit 'doom-modeline-buffer-major-mode))
  (defun cc/doom-modeline-setup-custom-modeline ()
    (doom-modeline-set-modeline 'cc 'default))
  (defun cc/doom-modeline-toggle-show-minor-modes ()
    (interactive)
    (if doom-modeline-minor-modes
        (setq doom-modeline-minor-modes nil)
      (setq doom-modeline-minor-modes t)))
  :config
  (setq inhibit-compacting-font-caches t
        doom-modeline-height 40
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-irc nil
        doom-modeline-major-mode-color-icon nil
        doom-modeline-modal-icon nil
        doom-modeline-mu4e nil
        doom-modeline-minor-modes nil
        doom-modeline-project-detection 'projectile)
  (doom-modeline-def-segment pad
    "Give more space"
    doom-modeline-spc)
  (doom-modeline-def-modeline 'cc
    '(bar
      workspace-name
      window-number
      modals
      matches
      buffer-info
      remote-host
      buffer-position
      word-count
      parrot
      selection-info)
    '(objed-state
      misc-info
      grip
      debug
      lsp
      minor-modes
      indent-info
      buffer-encoding
      major-mode
      process
      checker
      vcs
      pad
      pad))
  (cc/doom-modeline-setup-theme))

;; (load-theme 'kaolin-light t)		; light
;; (load-theme 'apropospriate-light t)	; light
;; (load-theme 'dracula t)                 ; dark
;; (load-theme 'nord t)			; dark
;; (load-theme 'night-owl t)               ; dark
(load-theme 'subatomic t)		; dark
(doom-modeline-mode)

;;; Handling projects
(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("H-p" . projectile-command-map))
  :custom
  (projectile-mode-line-prefix " ")
  (projectile-project-search-path '("~/code"))
  (projectile-indexing-method 'hybrid)
  (projectile-generic-command "rg --files --hidden --null")
  :init
  (projectile-mode +1))

(use-package ag)
(use-package rg
  :init
  (rg-enable-default-bindings))

;;; Completion system
(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-insert)
              ("C-l" . backward-kill-word))
  :custom
  (vertico-cycle t "Enable cycling for `vertico-next` and `vertico-previous`")
  (vertico-resize t "Grow and shrink the Vertico minibuffer")
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(basic partial-completion orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;; https://github.com/minad/marginalia#adding-custom-annotators-or-classifiers
(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Hide commands in M-x which do not work in the current
  ;; mode. Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Example configuration for Consult
(use-package consult
  :bind (;; C-c bindings
         ("C-c o h" . consult-history)
         ("C-c o m" . consult-mode-command)
         ("C-c o b" . consult-bookmark)
         ("C-c o k" . consult-kmacro)
         ;; C-x bindings
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x C-b" . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("C-h a" . consult-apropos)               ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)              ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  :init
  ;; Optionally configure the register formatting. This improves the
  ;; register preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Default preview after 0.2 seconds, for some candidates preview instantly
  ;; (setq consult-preview-key '(:debounce 0.2 any))
  (setq consult-preview-key 'any)

  ;; ;; Optionally configure the narrowing key.
  ;; ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; ;; Optionally make narrowing help available in the minibuffer.
  ;; ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  (setq consult-project-root-function #'projectile-project-root)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Optionally tweak the register preview window. This adds thin
  ;; lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced
  ;; version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-," . embark-dwim)
         ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :requires (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Version control
(use-package magit
  :bind (("C-c g s" . magit-status)
         ("C-c g f" . cc/git-add-with-force-current-buffer)
         ("H-s" . magit-status))
  :custom
  (magit-section-visibility-indicator nil)
  ;; NOTE: shackle prevents transient-display-buffer-action to do it's job. I
  ;; will keep it anyway in case shackle is removed. Shackle must handle the
  ;; display of transient buffers.
  (transient-display-buffer-action '(display-buffer-below-selected))
  :config
  (transient-bind-q-to-quit)
  :preface
  (defun cc/git-add-with-force-current-buffer ()
    "Track (git add -f) the file of the current buffer."
    (interactive)
    (shell-command (concat "git add -f " (shell-quote-argument buffer-file-name)))))

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine)
         ("H-t" . git-timemachine)))

(use-package git-gutter
  :bind (("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g r" . git-gutter:revert-hunk)
         ("C-c g a" . git-gutter:stage-hunk)
         ("C-c g u" . git-gutter:update-all-windows))
  :blackout t
  :hook ((after-load-theme . cc/git-gutter-setup-theme))
  :preface
  (defun cc/git-gutter-setup-theme ()
    "Configure git gutter in accordance to the current theme."
    (let ((default-fg (face-attribute 'default :foreground))
          (default-bg (face-attribute 'default :background)))
      (set-face-attribute 'fringe nil
                          :foreground default-bg
                          :background default-bg)
      (set-face-attribute 'git-gutter:separator nil
                          :foreground default-bg
                          :background default-bg)
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
  :custom
  (git-gutter:window-width 2)
  (git-gutter:added-sign "\uf067")
  (git-gutter:deleted-sign "\uf068")
  (git-gutter:modified-sign "\uf054")
  (git-gutter:hide-gutter nil)
  (global-git-gutter-mode 1)
  :config
  (cc/git-gutter-setup-theme))

;;; Terminal
(use-package vterm
  :hook ((vterm-mode . #'cc/vterm-setup))
  :bind (("H-v" . #'cc/project-vterm-other-window))
  :custom
  (vterm-max-scrollback 32768)
  :preface
  ;; NOTE: vterm build sometimes gets stuck, build manually and then restart Emacs
  ;; $ cd ./straight/repos/emacs-libvterm
  ;; $ mkdir build && cd build
  ;; $ cmake
  ;; $ make
  ;;
  ;; TODO: command to create a terminal for project `*$<PROJECT-NAME>-<NAME>*`
  ;; add buffer to project's buffer
  ;; display all terminals per current project
  ;; close terminal buffers when closing project
  ;; with `C-u` ask for project directory where to start shell
  ;; look at vterm-toggle
  (defun cc/vter--setup ()
    (setq-local global-hl-line-mode nil)
    (setq-local line-spacing nil)))

;;; PDF reader
(use-package pdf-tools
  :config
  (pdf-tools-install)
  :custom
  (pdf-view-use-scaling nil)
  (pdf-view-use-unicode-ligther nil))

;;; Other
(use-package scratch)

;;; Built-In packages configuration
(use-package dired
  :straight (:type built-in)
  :bind (("C-x C-j" . dired-jump))
  :custom
  (dired-listing-switches "-aGFhlv --color --group-directories-first --time-style=long-iso")
  (dired-dwim-target t))

;;; Universal minor modes
(use-package tree-sitter
  ;; enable language by language by calling tree-sitter-mode && tree-sitter-hl-mode
  :diminish " "
  :demand t)

(use-package tree-sitter-langs
  :requires tree-sitter)

(use-package whitespace
  :hook (before-save . whitespace-cleanup))

(use-package uniquify
  :straight (:type built-in)
  :config
  (setq-default uniquify-buffer-name-style 'forward))


(defun cc/color-lerp (a b t)
  "Linear interpolation between two colors.

Linear interpolation between color A and B in HEX format.

Parameter T is a float between 0 and 1, 0 means the result color
will be A-RGB, and 1 means the result color will be B-RGB."
  (require 'color)
  (let ((a-rgb (color-name-to-rgb a))
        (b-rgb (color-name-to-rgb b))
        (c-rgb))
    (pcase-let ((`(,a-r ,a-g ,a-b) a-rgb)
                (`(,b-r ,b-g ,b-b) b-rgb))
      (apply 'color-rgb-to-hex
       (list
        (+ a-r (* (- b-r a-r) t))
        (+ a-g (* (- b-g a-g) t))
        (+ a-b (* (- b-b a-b) t))
        2)))))


(use-package flymake
  :hook ((flymake-mode . cc/flymake--theme-setup)
         (after-load-theme . cc/flymake--theme-setup))
  :preface
  (defun cc/flymake--theme-setup ()
    (let ((foreground (face-attribute 'default :foreground))
          (background (face-attribute 'default :background)))
      (set-face-attribute 'flymake-error nil
                          :box `(:line-width 1 :color ,foreground :style nil)
                          :underline nil)
      ;; less standing up than error, more towards background
      (set-face-attribute 'flymake-warning nil
                          :box `(:line-width 1 :color ,(cc/color-lerp foreground background 0.2) :style nil)
                          :underline nil)
      ;; less standing up than warning, more towards background
      (set-face-attribute 'flymake-note nil
                          :box `(:line-width 1 :color ,(cc/color-lerp foreground background 0.4) :style nil)
                          :underline nil)
      (setq flymake-error-bitmap nil
            flymake-warning-bitmap nil)
      (message "Flymake OK!"))))

(use-package which-key
  :config
  (setq which-key-idle-delay 3)
  (which-key-mode))

(use-package prettier-js
  :blackout (prettier-js-mode . " ☰")
  :hook ((js-mode json-mode css-mode yaml-mode markdown-mode typescript-mode) . prettier-js-mode))

(use-package flycheck
  :blackout t
  :hook ((after-load-theme . cc/flycheck-setup-theme)
         (flycheck-mode . cc/flycheck-setup-theme))
  :preface
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
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-idle-change-delay 0.5)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-indication-mode nil))

(use-package flyspell
  :blackout t
  :hook ((text-mode . flyspell-mode)
         (after-load-theme . cc/flyspell-setup-theme)
         (flyspell-mode . cc/flyspell-setup-theme))
  :preface
  (defun cc/flyspell-setup-theme ()
    (set-face-attribute 'flyspell-incorrect nil
                        :underline '(:color "tomato" :style line))
    (set-face-attribute 'flyspell-duplicate nil
                        :underline '(:color "goldenrod" :style line)))
  :custom
  (flyspell-issue-message-flag nil)
  :config
  (setq ispell-program-name "/usr/bin/aspell")
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-," flyspell-mode-map)
  (unbind-key "C-;" flyspell-mode-map))

;; (use-package display-line-numbers
;;   :straight (:type built-in)
;;   :hook (prog-mode . display-line-numbers-mode)
;;   :config
;;   (setq-default display-line-numbers-width 3))

(use-package highlight-indent-guides
  :hook ((yaml-mode . highlight-indent-guides-mode)
         (json-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))

(use-package hydra)

(use-package use-package-hydra)

(use-package direnv
  :demand t
  :config (direnv-mode))

(use-package drag-stuff
  :blackout (drag-stuff-mode . " ")
  :config
  (setq drag-stuff-except-modes '(org-mode))
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

(use-package page-break-lines
  :blackout t
  :demand t
  :config
  (global-page-break-lines-mode))

(use-package expand-region
  :bind (("M-]" . er/expand-region)
         ("M-[" . er/contract-region)))

(use-package yasnippet
  :blackout (yas-minor-mode . "")
  :config (yas-global-mode)
  :custom
  (yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  (yas-new-snippet-default nil))

(use-package multiple-cursors
  :requires hydra
  :bind (("M-\\" . mc/mark-next-like-this)
         ("C-c m" . hydra-multiple-cursors/body)
         :map mc/keymap
         ("C-'" . mc-hide-unmatched-lines-mode))
  :hydra (hydra-multiple-cursors (:hint nil)
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
                                 ("q" nil)))

(use-package visual-regexp
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))

(use-package rainbow-mode
  :blackout t)

(use-package string-inflection
  :bind (("C-*" . string-inflection-all-cycle)
         ("C-c q l" . string-inflection-lower-camelcase)
         ("C-c q c" . string-inflection-camelcase)
         ("C-c q u" . string-inflection-underscore)
         ("C-c q U" . string-inflection-upcase)
         ("C-c q k" . string-inflection-kebab-case)))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;;; Smart stuff
(use-package company
  :blackout t
  :bind (("C-<tab>" . company-complete)
         :map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("M-." . company-show-location)
         ("<tab>" . company-complete-common-or-cycle)
         ("C-s" . company-search-candidates)
         ("C-d" . company-show-doc-buffer)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-require-match 'never)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-dabbrev-other-buffers nil)
  (company-backends '(company-nxml
                      company-css
                      company-capf
                      company-dabbrev-code
                      company-files
                      company-dabbrev)))

(use-package dabbrev
  :straight (:type built-in)
  :custom
  (dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (dabbrev-backward-only nil)
  (dabbrev-case-distinction 'case-replace)
  (dabbrev-case-fold-search nil)
  (dabbrev-case-replace 'case-replace)
  (dabbrev-check-other-buffers t)
  (dabbrev-eliminate-newlines t)
  (dabbrev-upcase-means-case-search t))

(use-package hippie-exp
  :straight (:type built-in)
  :config
  (global-set-key (kbd "M-/") 'hippie-expand)
  :custom
  ;; TODO
  ;; fix try-expand-line of unbalanced s-expressions
  ;; see: https://www.emacswiki.org/emacs/HippieExpand
  ;; see: https://gist.github.com/magnars/4060654
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line)))

;;; TODO: How to display parameters doc in overlay instead of minibuffer
;;; https://github.com/emacs-lsp/lsp-mode/issues/2749

;;; TODO: Trouble with lsp-mode and eldoc
;;; https://www.reddit.com/r/emacs/comments/fxqfs2/trouble_with_lspmode_and_eldoc
;;; check `lsp-signature-auto-activate` and `lsp-signature-render-documentation`
(use-package lsp-mode
  :bind (:map lsp-mode-map
         ("M-." . lsp-find-definition)
         ("M-?" . lsp-find-references))
  :custom
  (read-process-output-max (* 1024 1024))
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-completion-enable t)
  (lsp-completion-provider :capf)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-diagnostics-provider :flycheck)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-hook nil)
  (lsp-enable-file-watchers t)
  (lsp-enable-indentation t)
  (lsp-enable-links t)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.5)
  (lsp-keep-workspace-alive nil)
  (lsp-keymap-prefix "H-l")
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-prefer-capf t)
  (lsp-print-io t)
  (lsp-log-io t)
  (lsp-semantic-tokens-enable nil)
  (lsp-signature-render-documentation t)
  (lsp-signature-auto-activate t))

(use-package lsp-ui
  :hook ((after-load-theme . cc/lsp-ui--setup-theme)
         (lsp-ui-mode . cc/lsp-ui--setup-theme))
  :bind (("H-l h d" . lsp-ui-doc-show)
         ("H-l h D" . lsp-ui-doc-hide)
         :map lsp-mode-map
         ("H-d" . lsp-describe-thing-at-point)
         ("H-a" . lsp-execute-code-action))
  :preface
  (defun cc/lsp-ui--setup-theme ()
    (let ((default-foreground (face-attribute 'default :foreground))
          (default-background (face-attribute 'default :background)))
      (set-face-attribute 'lsp-details-face nil
                          :height 2)
      (set-face-attribute 'lsp-ui-doc-background nil
                          :foreground default-foreground
                          :background default-background)
      (set-face-attribute 'lsp-ui-doc-header nil
                          :foreground default-foreground
                          :background default-background
                          :weight 'bold
                          :height 2
                          :slant 'italic)
      (setq lsp-ui-doc-border default-foreground)))
  :custom
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-delay 1)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-height 600)
  (lsp-ui-doc-max-width 400)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-doc-show-with-cursor t)
  ;; increase the following during workshops
  (lsp-ui-doc-text-scale-level 1)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t))

(use-package consult-lsp
  :requires (lsp-mode consult marginalia)
  :bind (("H-l s d" . consult-lsp-diagnostic)
         ("H-l s s" . consult-lsp-symbols)
         ("H-l s S" . consult-lsp-file-symbols)
         ("M-g d" . consult-lsp-diagnostics)
         ("M-g s" . consult-lsp-symbols)
         ("M-g S" . consult-lsp-file-symbols))
  :hook (consult-lsp . consult-lsp-marginalia-mode))

(use-package consult-yasnippet
  :requires yasnippet)

;;; Handling buffers and windows
(use-package popper
  :bind (("C-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-mode-line '(:eval (propertize " POP " 'face 'mode-line-emphasis)))
  (popper-display-control nil)
  (popper-group-function #'popper-group-by-projectile)
  (popper-reference-buffers
        '("Output\\*$"
          "\\*Async Shell Command\\*"
          messages-buffer-mode
          vterm-mode
          help-mode
          rg-mode
          ag-mode
          compilation-mode))
  :config
  (popper-mode)
  (popper-echo-mode -1))

(use-package shackle
  :custom
  (shackle-default-size 0.4)
  (shackle-default-alignment 'below)
  (shackle-select-reused-winqdows nil)
  (shackle-rules '((compilation-mode :align below :size 0.3)
                   (help-mode :select t :align right)
                   (messages-buffer-mode :select nil :align right)
                   (rg-mode :select t :align right)
                   (ag-mode :select t :align right)
                   (grep-mode :select t :align right)
                   (occur-mode :select t :align right)
                   (vterm-mode :select t :align below :size 0.2)
                   (flycheck-error-list-mode :size 0.3 :align below)
                   ("*ert*" :align below :size 0.2)
                   ("*Backtrace*" :size 0.3 :align right)
                   ("*Warnings*" :ignore t)
                   ("*Embark Actions*" :size 0.3 :align right)
                   ("*info*" :custom (lambda (buffer alist _plist)
                                       ;; don't know why it doesn't work with plain shackle rules
                                       (push (cons 'side 'right) alist)
                                       (push (cons 'window-width 'fit-window-to-buffer) alist)
                                       (display-buffer-in-side-window buffer alist)))
                   ("^\\magit-diff: .*$" :regexp t :other t :select nil)
                   ("^\\s-*\\*transient\\*$" :regexp t :custom (lambda (buffer alist _plist)
                                                                 (display-buffer-below-selected buffer alist)))
                   ;; TODO: complete this
                   ))
  :config
  (shackle-mode t))

        ;;   ("*Completions*" :size 0.3 :align 'below :autoclose t)
        ;;   ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
        ;;   ("*ert*" :align 'below :autoclose t)
        ;;   ("*Messages*" :size 0.3 :align 'below :autoclose t)
        ;;   ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
        ;;   ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
        ;;   ("*Calendar*" :select t :size 0.3 :align 'below)
        ;;   (" *undo-tree*" :select t)
        ;;   ("*Paradox Report*" :size 0.3 :align 'below :autoclose t)
        ;;   ("*quickrun*" :select t :size 15 :align 'below)
        ;;   ("*tldr*" :align 'below :autoclose t)
        ;;   ("*Youdao Dictionary*" :size 0.3 :align 'below :autoclose t)
        ;;   ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)

        ;;   (grep-mode :select t :align 'below)
        ;;   (ivy-occur-grep-mode :select t :align 'below)
        ;;   (pt-mode :select t :align 'below)
        ;;   (rg-mode :select t :align 'below)

        ;;   (flycheck-error-list-mode :select t :size 0.3 :align 'below :autoclose t)
        ;;   (flymake-diagnostics-buffer-mode :select t :size 0.3 :align 'below :autoclose t)

        ;;   (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
        ;;   (comint-mode :align 'below)
        ;;   (helpful-mode :select t :size 0.4 :align 'below :autoclose t)
        ;;   (process-menu-mode :select t :size 0.3 :align 'below :autoclose t)
        ;;   (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
        ;;   (profiler-report-mode :select t :size 0.5 :align 'below)
        ;;   (tabulated-list-mode :align 'below))))



;;; JavaScript/TypeScript
(use-package js
  :straight (:type built-in)
  :hook ((js-mode . cc/javascript--setup))
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . javascript-mode))
  (defun cc/javascript--setup ()
    (tree-sitter-mode)
    (tree-sitter-hl-mode)
    (show-paren-mode -1)
    (prettier-js-mode)
    (flycheck-mode)
    (lsp)
    (lsp-ui-mode)
    (when (and (executable-find "eslint") (not (eq major-mode 'json-mode)))
      ;; FIX: make flycheck run eslint and report all the lsp problems it's not
      ;; working as expected. An appropriate solution would be to have the lsp
      ;; server to run `eslint`, suprisingly it's not doing it, probably we can
      ;; configure it. When done the only flycheck checker would be `lsp`. Right
      ;; know `eslint` reported problems are way better than the ones reported
      ;; by lsp.

      ;; (flycheck-add-next-checker 'lsp 'javascript-eslint)
      ;; (flycheck-select-checker 'javascript-eslint)
      (lsp-diagnostics-flycheck-enable)
      )
    (company-mode))
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset t))

;; TODO: expand "+N overloads" in lsp documentation
(use-package typescript-mode
  :hook ((typescript-mode . cc/typescript--setup))
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (defun cc/typescript--setup ()
    (tree-sitter-mode)
    (tree-sitter-hl-mode)
    (prettier-js-mode)
    (flycheck-mode)
    (company-mode)
    (lsp)
    (lsp-ui-mode))
  :custom
  (typescript-indent-level 2))

;;; C/C++
(use-package c
  :straight (:type built-in)
  :hook ((c-mode . cc/c--setup))
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode))
  :bind (:map c-mode-map
         ("C-c C-c" . compile))
  :preface
  (defun cc/c--setup ()
    (message "cc/c--setup")
    (add-hook 'after-save-hook 'lsp-format-buffer 0 t)
    (flycheck-mode)
    (company-mode)
    (lsp)
    (lsp-ui-mode)
    (lsp-semantic-tokens-mode -1)))

;;; TODO: see https://clangd.llvm.org/features
;;; .clang-format file
;;; .clang-tidy file
;;; Compilation flags???

;; (setq haskell-prompt-regexp "^\\(> *\\|| *\\)+")

;;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . cc/haskell--setup)
  :custom
  (haskell-prompt-regexp "^\\(> *\\|| *\\)+")
  :preface
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
  (defun cc/haskell-interactive-start ()
    "Start an interactive session loading current buffer.

Whenever the buffer is saved it will be also reloaded in the
current interactive session."
    (interactive)
    ;; TODO: is there another way to save current point and window and
    ;; restore it when you invoke functions that select other windows to
    ;; operate?
    (let ((buffer (current-buffer)))
      (save-excursion
        (haskell-interactive-bring))
      (haskell-process-load-file)
      (switch-to-buffer-other-window buffer))
    (add-hook 'after-save-hook 'haskell-process-reload nil t))
  :config
  (setq haskell-process-type 'ghci)
  (setq haskell-process-path-ghci (executable-find "stack"))
  (setq haskell-process-args-ghci '("ghci"))
  ;; (setq haskell-prompt-regexp "^\\([>|] *\\)+")
  (setq haskell-prompt-regexp "^\\(> *\\)+")
  (setq inferior-haskell-root-dir "/tmp"))

(use-package lsp-haskell
  :custom
  (lsp-log-io nil))


;;; Emacs Lisp
(use-package emacs-lisp
  :straight (:type built-in)
  :bind (:map emacs-lisp-mode-map
         ("C-M-<backspace>" . backward-kill-sexp))
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("*scratch*" . emacs-lisp-mode))
  :hook ((emacs-lisp-mode . cc/emacs-lisp--setup))
  :config
  (require 'emacs-lisp-functions)
  :preface
  (defun cc/emacs-lisp--setup ()
    (setq-local flycheck-emacs-lisp-load-path 'inherit)
    (flycheck-mode)
    (company-mode)
    (eldoc-mode)))

(use-package cask-mode)

(use-package eval-sexp-fu
  :blackout t
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode)
  :init
  (use-package highlight)
  (setq eval-sexp-fu-flash-face 'widget-field)
  (setq eval-sexp-fu-flash-error-face 'font-lock-warning-face)
  (setq eval-sexp-fu-flash-duration 0.5)
  :config
  (esf-initialize))

(use-package paredit
  :diminish (paredit-mode . " (P)")
  :hook (emacs-lisp-mode . enable-paredit-mode))

;;; Makefile
(use-package makefile-executor
  ;; TODO: check if C-c C-c is already bound or not, if not and there's a
  ;;       Makefile in the project then bind C-c C-c to
  ;;       makefile-executor-execute-last
  :hook (makefile-mode . makefile-executor-mode))

;;; JSON
(use-package json-mode
  :mode (("\\.json\\'"  . json-mode)
         (".babelrc" . json-mode)
         (".prettierrc" . json-mode)
         (".eslintrc" . json-mode))
  :hook (json-mode . cc/json--setup)
  :preface
  (defun cc/json--setup ()
    (prettier-js-mode)
    (flycheck-mode)
    (flycheck-disable-checker 'javascript-eslint)
    (when (executable-find "jsonlint")
      (flycheck-select-checker 'json-jsonlint)))
  :config
  (setq js-indent-level 2))

;;; YAML
(use-package yaml-mode
  ;; TODO: https://github.com/redhat-developer/yaml-language-server
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.neon\\'" . yaml-mode)))

;;; CSV
(use-package csv-mode)

;;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom-face (markdown-code-face ((t (:inherit nil))))
  :init
  (setq markdown-fontify-code-blocks-natively nil)
  (setq markdown-command "pandoc --from markdown_github -t html5 -s"))

;;; GraphQL
(use-package graphql-mode)

;;; Erlang
(use-package
  erlang
  :mode "\\.erl$")

;;; Elixir
(use-package eglot)

;;; TODO: extract somewhere else
(defun cc/flymake-diagnostic-text-at-point ()
  "Return flymake diagnostic text at point."
  (flymake--diag-text (get-char-property (point) 'flymake-diagnostic)))

;;; TODO: extract somewhere else
;;; TODO: show in a buffer with custom highlights?
(defun cc/flymake-show-diagnostic-text-at-point ()
  "Show flymake diagnostic text at point in a buffer."
  (interactive)
  (message (cc/flymake-diagnostic-text-at-point)))

(use-package elixir-mode
  :hook ((elixir-mode . cc/elixir--setup))
  :bind (:map elixir-mode-map
         ("H-e" . #'cc/eldoc-doc-buffer-and-select)
         ("H-f" . #'cc/flymake-show-diagnostic-text-at-point)
         :map flymake-mode-map
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error))
  :custom
  (eldoc-echo-area-use-multiline-p 5)
  (eldoc-echo-area-display-truncation-message "...")
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  :preface
  (defun cc/elixir--setup ()
    (flymake-mode)
    (company-mode)
    (tree-sitter-mode)
    (tree-sitter-hl-mode)
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (eglot-ensure))
  (defun cc/eldoc-doc-buffer-and-select ()
    (interactive)
    (eldoc-doc-buffer t)
    (let ((w (get-buffer-window "*eldoc*")))
      (message "Window: %s" w)
      (when w
        (select-window w)))))

;;; TODO: to be able to run tests in docker, needs to be tested, maybe a pull request after?

;; (setq apprentice-mix-command "docker-compose exec offer-engine mix")

;; (defun cc/file-path-relative-to-project-root (file-path)
;;   "Convert FILE-PATH to relative path to current project root."
;;   (if (file-exists-p file-path)
;;       file-path
;;     (let ((pr (expand-file-name (project-root (project-current)))))
;;       (if (and pr (string-prefix-p pr file-path))
;;           (string-remove-prefix pr file-path)
;;         file-path))))

;; (defun cc/advice--apprentice-mix--execute-test (&optional what)
;;   "Filter WHAT..."
;;   (cond ((not what) '())
;;         ((string-search ":" what)
;;          (let* ((tokens (split-string what ":"))
;;                 (path (car tokens))
;;                 (line (cadr tokens)))
;;            (list (concat (cc/file-path-relative-to-project-root path) ":" line))))
;;         (t (list (cc/file-path-relative-to-project-root what)))))

;; (advice-add
;;  #'cc/advice--apprentice-mix--execute-test
;;  :filter-args
;;  apprentice-mix--execute-test)

(use-package apprentice
  :straight (apprentice :type git :host github :repo "Sasanidas/Apprentice")
  :hook ((elixir-mode . apprentice-mode)))
;;; TODO: mix commands
;;; TODO: exunit commands like run this test
;;; TODO: eglot ls customization parameters
;;; TODO: complex layout -> primary buffer | supporting buffer -> complex layout. is this possible for stuff like ripgrep?

;; (use-package
;;   exunit
;;   :ensure t
;;   :after elixir-mode
;;   :commands (anil/mix-format)
;;   :bind
;;   (:map elixir-mode-map
;;         ("C-c , a" . exunit-verify-all)
;;         ("C-c , A" . exunit-verify-all-in-umbrella)
;;         ("C-c , s" . exunit-verify-single)
;;         ("C-c , v" . exunit-verify)
;;         ("C-c , r" . exunit-rerun)
;;         ("C-c , t" . exunit-toggle-file-and-test)
;;         ("s-r" . exunit-rerun)
;;         )
;;   (:map elixir-mode-map
;;         ("C-c i f" . anil/mix-format))
;;   (:map exunit-compilation-mode-map
;;         ("C-o" . ace-window))
;;   :config
;;   (defun anil/mix-format ()
;;     (interactive)
;;     (save-buffer)
;;     (shell-command (format "cd %s && mix format %s"
;;                            (or
;;                             (ignore-errors (exunit-umbrella-project-root))
;;                             (exunit-project-root))
;;                            (buffer-file-name)))
;;     (revert-buffer t t))
;;   )

;; (use-package
;;   flycheck-credo
;;   :after (flycheck elixir-mode)
;;   :custom (flycheck-elixir-credo-strict t))

;; (use-package
;;   flycheck-dialyxir
;;   :ensure t
;;   :after elixir-mode)

;; (use-package
;;   flycheck
;;   :no-require t
;;   :init (flycheck-dialyxir-setup))

;; (use-package
;;   inf-elixir
;;   :ensure t
;;   :after elixir-mode
;;   :bind (("C-c i i" . 'inf-elixir)
;;          ("C-c i p" . 'inf-elixir-project)
;;          ("C-c i l" . 'inf-elixir-send-line)
;;          ("C-c i r" . 'inf-elixir-send-region)
;;          ("C-c i b" . 'inf-elixir-send-buffer)))

;; (use-package
;;   mix
;;   :ensure t
;;   :after elixir-mode
;;   :hook (elixir-mode . mix-minor-mode))

;;; Org
(use-package org
  :bind (("C-c c" . org-capture)
         ("C-M-<return>" . org-insert-todo-subheading)
         :map org-mode-map
         ("M-<down>" . org-metadown)
         ("M-<up>" . org-metaup)
         ("M-<right>" . org-demote-subtree)
         ("M-<left>" . org-promote-subtree)
         ("C-x c s" . org-cut-subtree)
         ("C-c C-x C-i" . org-clock-in))
  :config
  (setq org-edit-src-content-indentation 0
        org-src-preserve-indentation nil
        org-src-tab-acts-natively t
        org-src-fontify-natively nil
        org-use-property-inheritance t
        org-confirm-babel-evaluate nil
        org-catch-invisible-edits 'error
        org-tags-column -100
        org-startup-indented t
        org-return-follows-link t
        org-src-fontify-natively t
        org-link-frame-setup '((file . find-file))
        org-support-shift-select 'always)
  (setq org-tag-persistent-alist '(("drill" . ?r)
                                   ("doing" . ?d)
                                   ("next" . ?n)
                                   ("today" . ?t)
                                   ("blocked" . ?b)
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
        ;; TODO: extract
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
     (js . t)
     (shell . t))))

;; (use-package ob-jq)
(use-package ob-http)
(use-package ob-mongo)
(use-package org-drill
  :requires org
  :commands org-drill
  :config
  (add-to-list 'org-modules 'org-drill)
  (setq org-drill-add-random-noise-to-intervals-p t
        org-drill-hint-separator "||"
        org-drill-left-cloze-delimiter "<["
        org-drill-right-cloze-delimiter "]>"
        org-drill-learn-fraction 0.25))

(use-package org-tree-slide
  :requires org
  :bind (("H-n" . org-tree-slide-move-next-tree)
         ("H-p" . org-tree-slide-move-previous-tree)
         :map org-mode-map
         ("C-c s" . org-tree-slide-mode))
  :config
  (org-tree-slide-simple-profile))

;;; TODO: Docker -> https://github.com/Silex/docker.el
;;; TODO: Haskell
;;; TODO: Rust (rust-mode, rustic, cargo)
;;; TODO: PureScript
;;; TODO: Kotlin
;;; TODO: Java
;;; TODO: Elixir
;;; TODO: Erlang
;;; TODO: Kubernetes
;;; TODO: Ansible
;;; TODO: Terraform
;;; TODO: Julia
;;; TODO: Dhall
;;; TODO: CommonLisp
;;; TODO: Racket
;;; TODO: TLA+

;;; Configuration dependent from installed packages
(require 'better-defaults-after)

(require 'personal-functions)
(require 'personal-bindings)

;;; TODO: Emacs, indent with yank text
;;; TODO: Emacs, disable documentation in minibuffer?

;;; Take inspiration from https://github.com/ianyepan/.wsl-emacs.d/blob/master/init.el
;;; Take inspiration from https://config.phundrak.com/emacs.html

;;; TODO: ("C-x b" . switch-to-buffer) ("C-x C-b" . projectile-switch-to-buffer)
;;; TODO: hydra problem when trying to create multiple cursors, try.
;;; TODO: emacs-lisp: company complete sort common prefix first.
;;; TODO: emacs-lisp: replace standard binding with M-s.
;;; TODO: emacs-lisp: when complete with abbrev do not paste unbalanced parentheses.
;;; TODO: emacs-lisp: when comment (commment-dwim?) find where the sexp end and comment the whole sexp by leaving balanced parentheses.
;;; TODO: emacs-lisp: when duplicate line with cc/duplicate-line-or-region check if line has unbalanced parentheses then try to fix them (ex. delete closed parentheses at the end)
;;; TODO: emacs-lisp: find a way to control Spotify within Emacs

;;; TODO: add https://www.emacswiki.org/emacs/spell-number.el

;;; TODO: org-mode: disable drag-stuff and enable native org-indent-item, org-move-item-up, org-move-item-down, org-outdent-item, ...

;;; https://blog.meain.io/2021/intelligent-snippets-treesitter/ ************* (how to use tree-sitter in minor modes)
;;; wgrep-ag
;;; wgrep-rg?
;;; TODO: *ag* preview in other window, how?
;;; TODO: capf-autosuggest for shells and comint?
;;; TODO: marmaid-mode
;;; TODO: Grammarly integration
;;; TODO: https://github.com/veelenga/carbon-now-sh.el || https://github.com/Aloxaf/silicon || https://github.com/tecosaur/screenshot
;;; TODO: How to change dictionary?

;; bind-key
;; uuidgen
;; request
;; forge
;; feature-mode
;; erlang
;; elixir-mode
;; mix
;; add-node-modules-path
;; haskell-mode
;; lsp-haskell
;; dhall-mode
;; cider
;; dockerfile-mode
;; docker-tramp
;; docker
;; svg-lib

;; TODO: make "starred buffers" (buffers without backing files?) to be ranked low in buffer candidates

;;; TODO: window setting management
;;; - https://github.com/tlh/workgroups.el
;;; - https://github.com/LouisKottmann/zygospore.el

;;; TODO: performance tuning
;;; TODO: install and configure `forge`

;;; TODO: nyxt browser

;;; TODO: when first commit then choose initial commit message from candidates
;;; TODO: package gist has too many errors???


;;; TODO: look at https://github.com/ebpa/tui.el *********************
;;; TODO: study what can be done during isearch sessions (ex. switch to "search and replace" or "search by regexp")
;;; TODO: study projectile documentation
;;; TODO: study embark documentation
;;; TODO: study isearch documentation
;;; TODO: study consult code
;;; TODO: study and exercise with macros
;;; TODO: study https://emacs-tree-sitter.github.io
;;; TODO: learn about https://cask.readthedocs.io/en/latest/

;;; TODO: company-yasnippet (from https://github.com/deech/.use-package.emacs.d/blob/master/init.el)
;;; TODO: how does it work?
;; (use-package yasnippet-snippets
;;   :config
;;   (yas-global-mode)
;;   (advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
;;   (advice-add 'company-complete-common :after (lambda () (when (equal my-company-point (point)) (yas-expand)))))

;;; TODO: function to quickly open/show the URL at point
;;;   - https://gist.github.com/synaptiko/46a63f46abed674cc696545b619f0413_(i3-msg)
;;;   - https://dev.to/elisealcala/start-a-new-electron-app-with-react-and-typescript-5f67
;;;   - https://github.com/diego3g/electron-typescript-react
;;; TODO: projectile + makefile => bind C-c C-c

;;; TODO: when fill a paragraph in comment consider a dot as the end
;;; of paragraph and an initial uppercase letter as the start of the
;;; paragraph

;;; TODO: stuff to make
;;; - try to use a buffer with square fonts as a grid of pixels to draw something
;;;   - KreativeSquare.ttf
;;;   - square.ttf
;;;   - https://github.com/opentypejs/opentype.js
;;; - remove all-the-icons and create a modeline with SVG icons
;;; - integrate with https://comby.dev/ (refactoring tool) (comby.el)
;;; - markdown focus mode by narrowing current "block" and navigate to next/previous/upper block always by narrowing
;;; - show documentation of symbol at point (depends on the mode - dash??? lsp???)
;;; - snitch-el: create GitHub/GitLab/... issues starting from TODO blocks in current buffer/project
;;;   - consult-snitch, see consult-todo
;;; - company-yas autocomplete snippets keywords
;;; - consult-projectile as counsel-projectile ******
;;; - consult-colors
;;; - consult-unicode
;;; - consult-todo
;;; - consult-popper (list all the POP buffers, on enter display the buffer accordingly to display-buffer-alist rules)
;;; - tree-sitter-drag-around (replace drag-stuff)
;;; - tree-sitter-expand-region (replace expand region)
;;;   - see https://github.com/meain/evil-textobj-tree-sitter
;;; - tree-sitter-closer (insert the closing token dwim)
;;; - tree-sitter-indent
;;;   - see https://codeberg.org/FelipeLema/tree-sitter-indent.el
;;;   - see https://github.com/jcs090218/tree-sitter-fold
;;; - theme from https://marketplace.visualstudio.com/items?itemName=sdras.night-owl (dark and light)
;;; - theme from https://marketplace.visualstudio.com/items?itemName=RobbOwen.synthwave-vscode
;;; - better HTTP client as https://marketplace.visualstudio.com/items?itemName=rangav.vscode-thunder-client

;;; see https://game-icons.net/ for cool SVG icons for modeline

;;; FIX `(set-face-attribute 'flyspell-incorrect nil :underline '(:color "red1" :style line))` does not work, waves instead of lines???

;;; See: https://github.com/bard/emacs-director#end-to-end-testing

;;; Take inspiration from:
;;; - https://depp.brause.cc/dotemacs/

;;; Shen EmacsLisp interpreter https://github.com/deech/shen-elisp/blob/master/shen-elisp.org ***************
;;; Code to learn from: https://code.librehq.com/qhong/crdt.el/-/blob/development/crdt.el

(message "Experimental configuration @%s done" user-emacs-directory)

(provide 'init)

;; TODO
;; I use the following for compilation. It keeps the compilation buffer if there
;; are warnings or errors, and buries it otherwise (after 1 second).
;;
;; (defun bury-compile-buffer-if-successful (buffer string)
;;  "Bury a compilation buffer if succeeded without warnings "
;;  (when (and
;;          (buffer-live-p buffer)
;;          (string-match "compilation" (buffer-name buffer))
;;          (string-match "finished" string)
;;          (not
;;           (with-current-buffer buffer
;;             (goto-char (point-min))
;;             (search-forward "warning" nil t))))
;;     (run-with-timer 1 nil
;;                     (lambda (buf)
;;                       (bury-buffer buf)
;;                       (switch-to-prev-buffer (get-buffer-window buf) 'kill))
;;                     buffer)))
;; Alternative
;; (defun bury-compile-buffer-if-successful (buffer string)
;;   "Bury a compilation buffer if succeeded without warnings "
;;   (if (and
;;        (string-match "compilation" (buffer-name buffer))
;;        (string-match "finished" string)
;;        (not
;;         (with-current-buffer buffer
;;           **(goto-char 1)**
;;           (search-forward "warning" nil t))))
;;       (run-with-timer 1 nil
;;                       (lambda (buf)
;;                         (bury-buffer buf)
;;                         (switch-to-prev-buffer (get-buffer-window buf) 'kill))
;;                       buffer)))
;; (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
