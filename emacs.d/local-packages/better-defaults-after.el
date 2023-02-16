;;; better-defaults-after.el --- summary -*- lexical-binding: t -*-

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

;; Configuration of built-in functionalities after loading external
;; packages

;;; Code:

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

(blackout 'auto-revert-mode)
(blackout 'eldoc-mode)

;; Indent pasted text
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (not (member major-mode '()))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; (add-to-list 'display-buffer-alist '(my-compile-goto-error-condition
;;                                      . (my-display-buffer-full-frame)))

(defun cc/goto-error-p (_buffer _action)
  "Is currently executed command a `next-error' or similar."
  (member this-command '(next-error previous-error compile-goto-error)))

(defun cc/main-window (&optional exclude)
  "Return the current main window.

The main window is the window with the bigger surface in the
current frame.

If the EXCLUDE function given a window returns 't then the window
is exluded as a main window competitor.

If no candidates remains returns nil."
  (->> (window-list)
       (-map (lambda (w) (cons w (* (window-width w) (window-height w)))))
       (-sort (lambda (wl wr) (> (cdr wl) (cdr wr))))
       (-remove (or exclude (lambda (w) nil)))
       (caar)))

(defun cc/main-window-no-compile ()
  "Return the current main window that is not in compile mode."
  (cc/main-window (lambda (w) (with-current-buffer (window-buffer (car w)) (derived-mode-p 'compilation-mode)))))

(defun cc/display-in-main-window-no-compile (buffer _action)
  "Display BUFFER in a window."
  (let (window (cc/main-window-no-compile))
    ;; (set-window-buffer window buffer)
    window))

;; (add-to-list 'display-buffer-alist '(cc/goto-error-p
;;                                      . (cc/display-in-main-window-no-compile)))
;; (pop display-buffer-alist)

;; (cc/main-window-no-compile)
;; (cc/main-window (lambda (w) (equal 'emacs-lisp-mode (with-current-buffer (window-buffer (car w)) major-mode))))
;; (cc/main-window (lambda (w) 't))

;; (eql (selected-window) (selected-window))

;; (buffer-file-name (window-buffer (selected-window)))

(provide 'better-defaults-after)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; better-defaults-after.el ends here
