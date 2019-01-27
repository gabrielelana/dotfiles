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

(provide 'org-functions)

;;; org-functions.el ends here
