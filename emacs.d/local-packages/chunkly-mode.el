;; TODO: shell command chunkly_backup -> compress ~/.chunkly directory, cp into dotfiles-secrets, comming and push

;; TODO: M-left reduce current start time of 5 minutes
;; TODO: M-right increase current start time of 5 minutes
;; TODO: M-p insert previous entry
;; TODO: before save check if chunk times are not interleaving otherwise mark them as errors
;; TODO: chunkly-edit-today opens the current day log
;; TODO: chunkly-edit-day asks for a day and then opens that day log
;; TODO: chunkly-edit-previous-day
;; TODO: chunkly-edit-next-day

(defvar chunkly-mode-hook nil)

(defvar chunkly-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "M-n") 'chunkly-insert-entry)
    map)
  "Keymap for Chunkly major mode")

(defun chunkly-insert-entry ()
  "Insert an entry in the chunkly log"
  (interactive)
  (let (previous-line next-line)
    (setq previous-line (chunkly--chomp (thing-at-point 'line t)))
    (when (string-empty-p previous-line)
      (save-excursion
        (forward-line -1)
        (setq previous-line (chunkly--chomp (thing-at-point 'line t)))))
    (setq next-line (if (or (null previous-line) (string-empty-p previous-line))
                        (message "XXX") ; chunkly--first-entry with questions
                      (chunkly--next-entry-given previous-line)))
    (end-of-line)
    (when (> (current-column) 1)
      (insert "\n"))
    (insert next-line)))

(defun chunkly--next-entry-given (previous-line)
  "Generate next entry in log given the previous line"
  (let* ((tokens (split-string previous-line "\t" t "[ \t\n]*"))
         (last-starting-time (nth 2 tokens))
         (last-duration (string-to-number (nth 4 tokens)))
         (last-description (nth 5 tokens))
         (next-starting-time (chunkly--next-chunk last-starting-time last-duration 300))
         (next-uuid (chunkly--generate-uuid)))
    (chunkly--format-entry next-uuid next-starting-time (number-to-string last-duration) last-description)))

(defun chunkly--format-entry (uuid starting-time duration description)
  "Format log entry"
  (string-join `(,uuid "L" ,starting-time "start" ,duration ,description) "\t"))

(defun chunkly--generate-uuid ()
  "Generate random UUID"
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6))))

(defun chunkly--next-chunk (from duration break)
  "Calculate and format the next chunk starting time"
  (let* ((from-time (date-to-time from))
         (next-time (time-add from-time (seconds-to-time (+ duration break)))))
    (format-time-string "%Y-%m-%dT%H:%M:%S%z" next-time "Europe/Rome")))

(defun chunkly--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(define-derived-mode chunkly-mode fundamental-mode "Chunkly"
  "Display and modify chunkly dump files"
  (use-local-map chunkly-mode-map))

(provide 'chunkly-mode)
