;;; org-functions.el --- summary -*- lexical-binding: t -*-

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

;; _

;;; Code:

(defun cc/org-mode-buffer-setup ()
  (advice-add 'org-babel-execute-src-block :after #'cc/org-babel-execute-src-block-pulse-momentary)
  (add-hook 'before-save-hook #'cc/org-mode-buffer-format nil t))

(defun cc/org-mode-buffer-format ()
  (cc/org-mode-buffer-force-uppercase-keywords))

(defun cc/org-mode-buffer-force-uppercase-keywords ()
  "Uppercase Org keywords and block identifiers."
  (interactive)
  ;; TODO: only if the current major mode is org-mode
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (re-search-forward "\\(?1:#\\+[a-z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
        (replace-match (upcase (match-string-no-properties 1)) :fixedcase nil nil 1)))))

(defun cc/org-babel-execute-src-block-pulse-momentary (&rest args)
  (let ((element (org-element-at-point))
        (original-pulse-delay pulse-delay))
    (when (eq (org-element-type element) 'src-block)
      (setq pulse-delay 0.05)
      (let* ((area (org-src--contents-area element))
             (starts-at (nth 0 area))
             (ends-at (nth 1 area)))
        (pulse-momentary-highlight-region starts-at ends-at))
      (setq pulse-delay original-pulse-delay))))

(provide 'org-functions)

;;; org-functions.el ends here
