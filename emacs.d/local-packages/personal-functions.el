;;; personal-functions.el --- summary -*- lexical-binding: t -*-

;; Author: Gabriele Lana
;; Maintainer: Gabriele Lana
;; Version: 0.0.1
;; Package-Requires: (_)
;; Homepage: http://github.com/gabrielelana/dotfiles
;; Keywords: dotfiles, configuration

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

;; Functions for everyday use

;;; Code:

(defun cc/pick-random (l)
  "Pick a random element from a list L."
  (nth (random (length l)) l))

(defun cc/recompile-packages ()
  "Recompile every package."
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
  "Remove all flyspell overlays on region between BEGIN and END."
  (require 'flyspell)
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
  "Open a new line below the cursor."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun cc/open-line-above ()
  "Open a new line above the cursor."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun cc/open-line-here ()
  "Splits the current line where the cursor is."
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
  "Kill the current buffer and deletes the file it is visiting."
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
  "Copy one character from DIRECTION line starting at point.

Copy the line above if DIRECTION is -1.
Copy the line below if DIRECTION is 1."
  (let (character-to-copy
        (origin (point))
        (column-where-to-copy (current-column)))
    (save-excursion
      (forward-line direction)
      (move-to-column column-where-to-copy)
      (setq character-to-copy (buffer-substring-no-properties (point) (1+ (point)))))
    (insert character-to-copy)))

(defun cc/shell-import-variables-from-dot-env-file (&optional directory-path)
  "Import in current process all environment defined in dot env file.

The dot env file is the one located at DIRECTORY-PATH if
specified or at the project root directory otherwise."
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

(defun cc/kill-current-file-path-with-line ()
  "Put current buffef file path with line in kill ring."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (line-number (line-number-at-pos))
         (project (project-current))
         (project-root (expand-file-name (project-root project))))
    (when file-path
      (kill-new
       (if (string-prefix-p project-root file-path)
           (concat (substring file-path (length project-root)) ":" (number-to-string line-number))
         (concat file-path ":" (number-to-string line-number)))))))

(defun cc/project-vterm-other-window (&optional terminal-name)
  "Create a terminal in other window named *{PROJECT-NAME}-{TERMINAL-NAME}*."
  (interactive "sName: ")
  (require 'vterm)
  (if (not (projectile-project-p))
      (error "ERROR: seems like you are not currently in a project")
    (let* ((-project-name (projectile-project-name))
           (-project-root-directory (projectile-project-root))
           (-current-directory default-directory)
           (-current-vterm-buffer-name vterm-buffer-name)
           (-terminal-buffer-name (format "*%s-%s*" -project-name terminal-name)))
      (unwind-protect
          (progn
            (setq default-directory -project-root-directory
                  vterm-buffer-name -terminal-buffer-name)
            (vterm-other-window -terminal-buffer-name)
            (c-update-modeline))
        (setq default-directory -current-directory
              vterm-buffer-name -current-vterm-buffer-name)))))

;;; Programming
(defmacro measure-time (name &rest body)
  "Measure the time it takes to evaluate BODY.

It will print \"{NAME}: xx.yyyyyys\"."
  (declare (indent defun))
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06fs" ,name (float-time (time-since time)))))

(defun buffer-base-name ()
  "Current buffer file name without directory and suffix."
  (->>
   (buffer-file-name)
   (file-name-without-extension)
   (file-name-without-directory)))

(defmacro todo (&rest body)
  "Declare that something has to be implemented.
No parameters or same arguments as #'error in BODY"
  (if (null body)
      `(error "UNIMPLEMENTED" )
    `(error ,(format "UNIMPLEMENTED: %s" message) ,@arguments)))

(defmacro unreacheable (&rest body)
  "If the execution flow reached this point something is wrong.
No parameters or same arguments as #'error in BODY"
  (if (null body)
      `(error "UNREACHEABLE" )
    `(error ,(format "UNREACHEABLE: %s" message) ,@arguments)))

(defmacro with-content-file (name content &rest body)
  "Create a unique file with a given CONTENT, eval BODY and delete it.

Generate a unique file. Store its path in symbol NAME. Write
CONTENT to the file. CONTENT can be a string or a list of strings
that will be concatenated with a newline. BODY will be evaluated,
therefore BODY can access the file through its path in NAME. No
matter what, at the end the file will be deleted."
  (declare (indent defun))
  (cl-assert (and name (symbolp name) t "NAME must be a symbol"))
  `(let ((,name (make-temp-file "wcf-macro")))
     (unwind-protect
         (cl-assert (or (stringp ,content) (seq-every-p 'stringp ,content)) t
                    "CONTENT must be a string or a list of strings")
       (progn (with-temp-file ,name
                (insert (mapconcat 'identity ,content "\n")))
              ,@body)
       (delete-file ,name))))

;;; I can get crazy for inconsistencies so...
(defalias 'nullp 'null)
(defalias 'atomp 'atom)
(defalias 'file-name-without-extension 'file-name-sans-extension)
(defalias 'file-name-without-directory 'file-name-nondirectory)

;;; Advices
(defadvice kill-region (around kill-word-or-kill-region activate)
  "Kill the active region or kill the previous word."
  (if (and (called-interactively-p "interactive") (not (region-active-p)))
      (backward-kill-word 1)
    ad-do-it))

(provide 'personal-functions)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; personal-functions.el ends here
