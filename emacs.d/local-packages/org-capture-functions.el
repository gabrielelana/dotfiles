;;; org-capture-functions.el --- org-capture utility functions -*- lexical-binding: t -*-

;; Author: Gabriele Lana
;; Maintainer: Gabriele Lana
;; Version: 0.0.1
;; Package-Requires: ()
;; Homepage: http://github.com/gabrielelana/dotfiles
;; Keywords: org-capture, emacs-lisp

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

;; In the beginning there was only darkness

;;; Code:

;;; TODO: (function (current-project-file+current-user-story ".project.org"))
;;; TODO: (function (current-project-file+ask-user-story-headline ".project.org"))
;;; TODO: command to jump to org-capture-last-stored instead of "C-x r b bookmark <RET>"
;;; TODO: rename ask to choose

;;; TODO: turn into a macro???
(defun current-project-file+ask-headline (file-name)
  (let* ((file-path (org-capture--current-project-file file-name))
         (headline (org-capture--ask-headline file-path)))
    (org-capture--goto-location file-path headline)))

(defun current-project-file+headline (file-name headline)
  (let* ((file-path (org-capture--current-project-file file-name)))
    (org-capture--goto-location file-path headline)))

(defun current-project-file+current-user-story (file-name)
  (let* ((file-path (org-capture--current-project-file file-name))
         (headline (org-capture--current-user-story-headline file-path)))
    (org-capture--goto-location file-path headline)))

(defun cc/org-capture-link-to-captured-line ()
  "Org file link to the line of the captured location.

The search path of the file is based on the line number, the
description contains the file path relative to the project path"
  (let* ((capture-buffer (org-capture-get :original-buffer))
         (capture-file-path (buffer-filename capture-buffer))
         (capture-file-line (with-current-buffer capture-buffer (number-to-string (line-number-at-pos))))
         (capture-file-relative-path (file-relative-name capture-file-path (projectile-project-root))))
    (concat
     "[[file://" capture-file-path "::" capture-file-line "][" capture-file-relative-path "::" capture-file-line "]]")))

(defun org-capture--current-project-file (file-name)
  (let* ((project-path (projectile-project-root))
         (file-path (concat project-path file-name)))
    (when (not (file-exists-p file-path))
      (error "file %S doesn't exists in project %S" file-name project-path))
    file-path))

(defun org-capture--ask-headline (file-path)
  (org-capture--choose-headline
   (org-capture--headline-candidates
    (org-headlines-from file-path))))

(defun org-capture--current-user-story-headline (file-path)
  (let ((headlines (org-headlines-from file-path ":doing:")))
    (if (nullp headlines)
        (error "No current User Story headline found in " file-path)
      (cdar headlines))))

(defun org-capture--choose-headline (candidates)
  (string-trim
   (completing-read
    "Choose the headline where the capture will go: "
    candidates
    nil
    nil
    nil)
   "[* ]\+"))

(defun org-capture--headline-candidates (headlines)
  (seq-map (lambda (headline)
             (concat (make-string (car headline) ?*) " " (cdr headline)))
           headlines))

(defun org-headlines-from (file-path &optional match)
  (with-current-buffer (find-file-noselect file-path)
    (org-map-entries 'org-headline-level-and-text-at-point (or match t) nil)))

(defun org-headline-level-and-text-at-point ()
  (let* ((components (org-heading-components))
         (level (nth 0 components))
         (text (nth 4 components)))
    (when (string-match "^\\(.*\\) \\[[^]]*\\]" text)
      (setq text (match-string 1 text)))
    (cons level text)))

(defun org-capture--goto-location (file-path headline)
  (set-buffer (org-capture-target-buffer file-path))
  (unless (derived-mode-p 'org-mode)
    (org-display-warning
     (format "Capture requirement: switching buffer %S to Org mode" (current-buffer)))
    (org-mode))
  (org-capture-put-target-region-and-position)
  (widen)
  (goto-char (point-min))
  (if (re-search-forward (format org-complex-heading-regexp-format (regexp-quote headline)) nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " headline "\n")
    (forward-line -1)
    (beginning-of-line)))

(provide 'org-capture-functions)

;;; org-capture-functions.el ends here
