;;; better-defaults-before.el --- summary -*- lexical-binding: t -*-

;; Author: Gabriele Lana
;; Maintainer: Gabriele Lana
;; Version: 0.0.1
;; Package-Requires: (_)
;; Homepage: http://github.com/gabrielelana/_
;; Keywords: _

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration of built-in functionalities

;;; Code:

;; Put custom configurations aside
(setq custom-file "~/.emacs.d/custom.el")
(when (not (file-exists-p custom-file))
  (write-region "" nil custom-file))
(load custom-file)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
(setq sentence-end-without-space ".!?$")
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t;#/]+[-+*] \\|[ \t;#/]\\*\\(TODO\\|FIX\\|NOTE\\)")
;; (setq paragraph-start "[ \t]*// - [^:]+:")
;;; from https://stackoverflow.com/questions/71788/getting-emacs-fill-paragraph-to-play-nice-with-javadoc-like-comments
;; (setq paragraph-start "^\\s-*\\#\\s-*\\\\\\(arg\\|ret\\).*$")

;; Don't show byte compile warnings
(setq byte-compile-warnings nil)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Hide me empty lines after buffer end
(set-default 'indicate-empty-lines nil)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Better scrolling
(setq scroll-conservatively 101) ; > 100
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)
(setq hscroll-step 1)
(setq hscroll-margin 0)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; No electric indent
(setq electric-indent-mode 1)

;; Nic says eval-expression-print-level needs to be set to nil (turned
;; off) so that you can always see what's happening.
;; Nic is wrong.
(setq eval-expression-print-level 100)

;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun cc/create-non-existent-directory ()
  "Offer to create parent directories if they do not exist."
  (let* ((parent-directory (file-name-directory buffer-file-name))
         (prompt (format "Directory `%s' does not exist! Create it?" parent-directory)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p prompt))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'cc/create-non-existent-directory)

;; Personal profile
(setq user-full-name "Gabriele Lana")
(setq user-mail-address "gabriele.lana@gmail.com")
(setq straight-host-usernames '((github . "gabrielelana")
                                (gitlab . "gabrielelana")
                                (bitbucket . "gabrielelana")))

;; Tramp
(setq tramp-terminal-type "dumb")
(setq tramp-default-method "ssh")
(setq tramp-remote-shell "/bin/sh")

;; Dont't backup files
(setq make-backup-files nil)
(setq create-lock-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Don't blink the cursor
(customize-set-variable 'blink-cursor-mode nil)

;; When scroll to the bottom/top then place the cursor to the very last/first line
(customize-set-variable 'scroll-error-top-bottom t)

;; Do not wait to have fully rendered the buffer before accepting inputs
(setq redisplay-dont-pause nil)

;; Highlight current line
(global-hl-line-mode)

;; Show matching parenthesis
(show-paren-mode)
(setq show-paren-delay 2)
(setq show-paren-style 'parenthesis)

;; More room in the macro's kill ring
(setq kmacro-ring-max 100)

;; More room for echo area, up to 75% of the current window
(setq max-mini-window-height 0.75)

;; Better performance sacrificing right-to-left languages
(setq-default bidi-display-reordering nil)

;; Underlining
(setq underline-minimum-offset 2)
(setq x-underline-at-descent-line t)

(provide 'better-defaults-before)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; better-defaults-before.el ends here
