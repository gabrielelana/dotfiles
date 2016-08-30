(require 's)

;; TODO: shell command chunkly_backup -> compress ~/.chunkly directory, cp into dotfiles-secrets, comming and push

;; TODO: M-p insert previous entry
;; TODO: before save check if chunk times are not interleaving otherwise mark them as errors

;; TODO: chunkly-edit-today, opens the current day log
;; TODO: chunkly-edit-day, asks for a day and then opens that day log
;; TODO: chunkly-edit-previous-day
;; TODO: chunkly-edit-next-day
;; TODO: chunkly-start-pomodoro, ask for description with default the last pomodoro

(defvar chunkly-mode-hook nil)

(defvar chunkly-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "M-n") 'chunkly-insert-related-entry)
    (define-key map (kbd "M-<left>") (lambda () (interactive) (chunkly--increase-starting-time -300)))
    (define-key map (kbd "M-<right>") (lambda () (interactive) (chunkly--increase-starting-time 300)))
    map)
  "Keymap for Chunkly major mode")

(defun chunkly-insert-related-entry ()
  "Insert an entry log related the current one if present"
  (interactive)
  (let ((current-entry (chunkly--current-entry))
        next-entry-line)
    (if (null current-entry)
        (error "chunkly-insert-entry not yet implemented")
      (setq next-entry-line (chunkly--format-entry (chunkly--guess-next-entry current-entry)))
      (save-excursion
        (end-of-line)
        (insert "\n" next-entry-line))
      (forward-line 1))))

(defun chunkly--increase-starting-time (number-of-seconds)
  "Adds seconds to the current starting time"
  (let ((current-entry (chunkly--current-entry)))
    (when current-entry
      (let* ((starting-time (nth 1 current-entry))
             (increased-starting-time (time-add starting-time (seconds-to-time number-of-seconds))))
        (setf (nth 1 current-entry) increased-starting-time)
        (save-excursion
          (delete-region (line-beginning-position) (line-end-position))
          (insert (chunkly--format-entry current-entry)))))))

(defun chunkly--current-entry ()
  (let ((current-entry-line (s-chomp (thing-at-point 'line t))))
    (if (s-blank? current-entry-line)
        nil
      (chunkly--parse-entry-line current-entry-line))))

(defun chunkly--parse-entry-line (entry-line)
  "Parse entry log line in its components"
  (let ((tokens (split-string entry-line "\t" t "[ \t\n]*")))
    (list
     (nth 0 tokens) ; uuid
     (date-to-time (nth 2 tokens)) ; starting-time
     (string-to-number (nth 4 tokens)) ; duration
     (nth 5 tokens) ; description
     )))

(defun chunkly--format-entry (entry)
  "Format entry log"
  (string-join `(,(nth 0 entry)
                 "L"
                 ,(format-time-string "%Y-%m-%dT%H:%M:%S%z" (nth 1 entry) "Europe/Rome")
                 "start"
                 ,(number-to-string (nth 2 entry))
                 ,(nth 3 entry)) "\t"))

(defun chunkly--guess-next-entry (previous-entry)
  "Generate next entry in log given the previous entry"
  (let ((next-entry (copy-sequence previous-entry))
        (previous-starting-time (nth 1 previous-entry))
        (previous-duration (nth 2 previous-entry))
        (break-duration 300)) ; TODO: extract as constant
    (setf (nth 0 next-entry) (chunkly--generate-uuid))
    (setf (nth 1 next-entry) (time-add previous-starting-time (seconds-to-time (+ previous-duration break-duration))))
    next-entry))

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

(define-derived-mode chunkly-mode fundamental-mode "Chunkly"
  "Display and modify chunkly dump files"
  (drag-stuff-mode -1)
  (use-local-map chunkly-mode-map))

(provide 'chunkly-mode)
