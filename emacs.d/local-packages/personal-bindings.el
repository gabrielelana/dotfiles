;;; personal-bindings.el --- summary -*- lexical-binding: t -*-

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

;; Personal bindings

;;; Code:

(require 'personal-functions)

(bind-key "RET" 'newline-and-indent)
(bind-key "H-p" #'cc/open-line-above)
(bind-key "H-n" #'cc/open-line-below)
(bind-key "H-<return>" #'cc/open-line-here)
(bind-key "M-<space>" #'rectangle-mark-mode)
(bind-key "H-u" #'cc/copy-character-from-above)
(bind-key "H-d" #'cc/copy-character-from-below)
(bind-key "H-<right>" #'windmove-swap-states-right)
(bind-key "H-<left>" #'windmove-swap-states-left)
(bind-key "H-<up>" #'windmove-swap-states-up)
(bind-key "H-<down>" #'windmove-swap-states-down)
(bind-key "H-+" #'text-scale-increase)
(bind-key "H--" #'text-scale-decrease)
(bind-key "C-a" #'cc/smarter-move-beginning-of-line)
(bind-key "C-c l" #'org-store-link)
(bind-key "C-^" #'cc/join-with-next-line)
(bind-key "C-;" #'cc/comment-or-uncomment-line-or-region)
(bind-key "C-x e" #'cc/eval-and-replace)
(bind-key "M-p" #'cc/duplicate-line-or-region-above)
(bind-key "M-n" #'cc/duplicate-line-or-region-below)
(bind-key "M-o" #'other-window)
(bind-key "C-M-y" #'yank-pop)
;; TODO ???
;; (bind-key "C-s" #'isearch-forward-regexp)
;; (bind-key "C-r" #'isearch-backward-regexp)
;; (bind-key "C-M-s" #'isearch-forward)
;; (bind-key "C-M-r" #'isearch-backward)
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
(global-set-key (kbd "C-c u l") "λ")
(global-set-key (kbd "C-c u a") "∧")
(global-set-key (kbd "C-c u o") "∨")
(global-set-key (kbd "C-c u >") "→")
(global-set-key (kbd "C-c u <") "←")
(global-set-key (kbd "C-c u =") "≡")
(global-set-key (kbd "C-c u b") "⊥")
(global-set-key (kbd "C-c u f") "∀")
(global-set-key (kbd "C-c u t") "⊤")

(provide 'personal-bindings)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; personal-bindings.el ends here
