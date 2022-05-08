(defvar custom/org-diary-directory "/home/journal/diary/"
  "Org Diary directory.")

(defvar custom/org-diary-time-format-file  "%d.%m.%Y"
  "Org Diary time format: file names.")

(defvar custom/org-diary-time-format-title "%d/%m/%Y"
  "Org Diary time format: entry titles.")

(defvar custom/org-diary-file-format (concat custom/org-diary-directory
					  custom/org-diary-time-format-file
					  ".org")
  "Org Diary file name format.")

(defvar custom/org-diary-open-in-new-window t
  "Open diary entries in new window.")

(defun custom/org-diary-init (time)
  "Set up Org Diary entry."
  (interactive)
  (insert (concat "#+title:" (custom/org-diary-time-string-title time) "\n"))
  (org-time-stamp-inactive '(16))
  (insert "\n\n"))

(defun custom/org-diary-parse-time (string)
  "Parse time string. Currently hardcoded to parse time
strings in the format `%d/%m/%Y'."
  (let ((dmy (cl-loop for n in (split-string string "/")
		            collect (string-to-number n))))
    (encode-time (list 0 0 0 (nth 0 dmy) (nth 1 dmy) (nth 2 dmy) nil nil nil))))

(defun custom/org-diary-time-string-file (time)
  (format-time-string custom/org-diary-file-format time))

(defun custom/org-diary-time-string-title (time)
  (format-time-string custom/org-diary-time-format-title time))

(defun custom/org-diary-buffer-entry (buffer)
  (string-match-p "^[0-9]\\{2\\}\\.[0-9]\\{2\\}\\.[0-9]\\{4\\}\\.org" (file-name-nondirectory buffer)))

(defun custom/org-diary-in-entry ()
  "Return t if current buffer is an `custom/org-diary-buffer-entry'."
  (custom/org-diary-buffer-entry buffer-file-name))

(defun custom/org-diary-entry-time ()
  (let ((title (custom/org-get-title-current-buffer)))
    (custom/org-diary-parse-time title)))

(defun custom/org-diary-entry-date ()
  "Retrieve the time of the current Org Diary
file in `custom/org-diary-time-format-file'."
  (custom/org-diary-time-string-file (custom/org-diary-entry-time)))

(defun custom/org-diary-entry-unsaved-buffer (time)
  "Return t if the Org Diary entry for TIME exists
in an unsaved buffer."
  (let ((file (custom/org-diary-time-string-file time)))
    (cl-loop for buffer in (buffer-list)
	           if (and (buffer-file-name buffer)
			   (string-equal file (buffer-file-name buffer))) return t
		   finally return nil)))

(defun custom/org-diary-browse ()
  "Org Agenda-like list of diary entries.
Options:
- org-recenter -> show diary at point in side by side window
- enter -> jump to diary entry at point
- quit -> quit and return to previous window config, buffer and visibility
  - org-agenda -> save current window config, visibility"
  (interactive))

(defun custom/org-diary-visit (time &optional number)
  "Open the Org Diary entry of the spencified time, creating
it if it does not exist."
  (interactive)
  (if number (setq time (time-add (days-to-time number) time)))
  (let ((entry (custom/org-diary-time-string-file time)))
    (if (or (file-exists-p entry) (custom/org-diary-entry-unsaved-buffer time))
	  (if custom/org-diary-open-in-new-window
	      (find-file-other-window entry)
	    (find-file entry))
      (progn (find-file entry)
	         (custom/org-diary-init time)))))

(defun custom/org-diary-today (&optional number)
  "Open the Org Diary entry for today, creating it if
it does not exist."
  (interactive)
  (custom/org-diary-visit (current-time) number))

(defun custom/org-diary-jump (number)
  (interactive)
  (let ((custom/org-diary-open-in-new-window (not (custom/org-diary-in-entry)))
	  (time-jump (time-add (custom/org-diary-entry-time) (days-to-time number))))
       (custom/org-diary-visit time-jump)))

(defun custom/org-diary-prior ()
  (interactive)
  (custom/org-diary-jump -1))

(defun custom/org-diary-next ()
  (interactive)
  (custom/org-diary-jump 1))

(global-set-key (kbd "C-c d") 'custom/org-diary-today)

(define-key org-mode-map (kbd "C-<prior>") 'custom/org-diary-prior)
(define-key org-mode-map (kbd "C-<next>") 'custom/org-diary-next)

(defun custom/org-diary ()
  "Org Diary minor mode.

Activate when visiting files matching pattern.

Bindings:
- C-<up>   -> previous entry if it exists
- C-<down> -> next entry if it exists
- C-n      -> new entry")

(provide 'org-diary)
