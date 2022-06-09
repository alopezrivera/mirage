;;; org-paragraph.el --- Paragraphs passing as items -*- lexical-binding: t -*-

;; Copyright (C) Antonio López Rivera

;; Author: Antonio Lópezr Rivera <antonlopezr99@gmail.com>
;; Maintainer: Antonio Lópezr Rivera <antonlopezr99@gmail.com>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/alopezrivera/org-paragraph

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org Mode parargaphs hitch-hiking their way to first class
;; structural editing citizenship.
;; org-paragraph works behind the scenes to make `org-metaright',
;; `org-metaleft', `org-metaup' and `org-metadown' work for
;; paragraphs.

;;; Code:

(defgroup custom/org-diary-mode-group nil
  "Group for customization"
  :prefix "custom/org-diary-")

(define-minor-mode custom/org-diary-mode
  "Org Diary minor mode."
  :init-value 1
  :lighter " Diary"
  :group 'custom/org-diary-mode-group

  (when (bound-and-true-p custom/org-diary-mode)
    (custom/org-diary-typeset))
  (when (not (bound-and-true-p custom/org-diary-mode))
    (custom/org-diary-font-lock-remove)
    (if custom/org-diary-variable-pitch
	   (variable-pitch-mode 0))))

(define-globalized-minor-mode custom/org-diary-global-minor-mode custom/org-diary-mode custom/org-diary-mode :group 'custom/org-diary-mode-group)

(defcustom custom/org-diary-directory "/home/diary/"
  "Org Diary directory"
  :group 'custom/org-diary-mode-group
  :type 'boolean)

(defcustom custom/org-diary-time-format-file  "%d-%m-%Y"
  "Org Diary time format: file names"
  :group 'custom/org-diary-mode-group
  :type 'string)

(defcustom custom/org-diary-time-format-title "%d/%m/%Y"
  "Org Diary time format: entry titles."
  :group 'custom/org-diary-mode-group
  :type 'string)

(defcustom custom/org-diary-new-window-fraction 0.25
  "New Org Diary window width as a fraction of the frame width"
  :group 'custom/org-diary-mode-group
  :type 'float)

(defcustom custom/org-diary-visit-in-new-window t
  "Whether to open diary entries in new window"
  :group 'custom/org-diary-mode-group
  :type 'boolean)

(defcustom custom/org-diary-variable-pitch nil
  "Whether to activate `variable-pitch-mode' in Org Diary entries"
  :group 'custom/org-diary-mode-group
  :type 'boolean)

(defcustom custom/org-diary-navigate-in-current-dir t
  "If the current buffer is an `org-diary' buffer, this variable
determines whether `org-diary-prior' and `org-diary-next' will
search (or create) the prior or next `org-diary' entry in the directory
of the current buffer as opposed to in `org-diary-directory'.
Setting this variable to t is useful to navigate directories with
notes in `org-diary' format"
  :group 'custom/org-diary-mode-group
  :type 'boolean)

(defun custom/org-diary-file-format (&optional dir)
  "Org Diary file name format."
  (let ((dir  (or dir
		     (if (and custom/org-diary-navigate-in-current-dir buffer-file-name)
			 (file-name-directory buffer-file-name)
		       custom/org-diary-directory)))
	   (file custom/org-diary-time-format-file))
  (concat dir file ".org")))

(defun custom/org-diary-new-window ()
  (split-window-horizontally)
  (windmove-right)
  (if (not (ignore-errors (custom/org-diary-resize-window)))
      (delete-other-windows)))

(defun custom/org-diary-resize-window ()
  (interactive)
  (custom/window-resize-fraction custom/org-diary-new-window-fraction 60))

(defun custom/org-diary-typeset ()
  (if custom/org-diary-variable-pitch
      (variable-pitch-mode))
  (custom/org-diary-font-lock-add))

(defface custom/org-diary-typeface-hhmm
  '((nil :foreground "#eb07b6" :inherit 'fixed-pitch))
  "Org Diary typeface for hh:mm time stamps."
  :group 'custom/org-diary-mode-group)

(defcustom custom/org-diary-keyword-hhmm '("[0-9]\\{2\\}:[0-9]\\{2\\}$" . 'custom/org-diary-typeface-hhmm)
  "Org Diary hh:mm typeface keyword."
  :group 'custom/org-diary-mode-group)

(defcustom custom/org-diary-keywords (list custom/org-diary-keyword-hhmm)
  "Org Diary font-lock keywords.")

(defun custom/org-diary-font-lock-add ()
  (font-lock-add-keywords nil custom/org-diary-keywords)
  (font-lock-fontify-buffer))

(defun custom/org-diary-font-lock-remove ()
  (font-lock-remove-keywords nil custom/org-diary-keywords)
  (font-lock-fontify-buffer))

(defun custom/org-diary-parse-time (string)
  "Parse time string. Currently hardcoded to parse time
strings in the format `%d/%m/%Y'."
  (let ((dmy (cl-loop for n in (split-string string "/")
		            collect (string-to-number n))))
    (encode-time (list 0 0 0 (nth 0 dmy) (nth 1 dmy) (nth 2 dmy) nil nil nil))))

(defun custom/org-diary-time-string-file (time &optional dir)
  (format-time-string (custom/org-diary-file-format dir) time))

(defun custom/org-diary-time-string-title (time)
  (format-time-string custom/org-diary-time-format-title time))

(defun custom/org-diary-buffer-entry (buffer)
  (string-match-p "^[0-9]\\{2\\}\\-[0-9]\\{2\\}\\-[0-9]\\{4\\}\\.org" (file-name-nondirectory buffer)))

(defun custom/org-diary-in-entry ()
  "Return t if current buffer is an `custom/org-diary-buffer-entry'."
  (ignore-errors (custom/org-diary-buffer-entry buffer-file-name)))

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
			 (string-match (buffer-file-name buffer) file))
		    return t
             finally return nil)))

(defun custom/org-diary-browse ()
  "Org Agenda-like list of diary entries.
Options:
- org-recenter -> show diary at point in side by side window
- enter -> jump to diary entry at point
- quit -> quit and return to previous window config, buffer and visibility
  - org-agenda -> save current window config, visibility"
  (interactive))

(defun custom/org-diary-open (entry &optional noselect new-window)
  "Open an Org Diary diary.

If a buffer for the entry exists, and the buffer is being displayed in a window,
switch to that window; otherwise, switch to that buffer.

- NOSELECT:   open entry file without selecting it
- NEW-WINDOW: open entry in new window"
  (setq entry-buffer (custom/find-buffer-by-file-name entry))
  (setq entry-window (if entry-buffer
			    (get-buffer-window entry-buffer)
		          nil))
  (cond (noselect                      (find-file-noselect entry))
	   (entry-window                  (select-window entry-window))
	   ((and entry-buffer new-window) (progn (custom/org-diary-new-window) (switch-to-buffer entry-buffer)))
	   (new-window                    (progn (custom/org-diary-new-window) (find-file        entry)))
	   (t                             (find-file entry))))

(defun custom/org-diary-visit (time &optional arg dir)
  "Open the Org Diary entry corresponding to the specified time, and initialize it if necessary.
-             '(0):  noselect
- C-u         '(4):  visit in current buffer
- C-u C-u     '(16): save new entry after initialiation
- C-u C-u C-u '(64): visit in current buffer and save new entry after initialization"
  (interactive)
  (let ((entry      (custom/org-diary-time-string-file time dir))
	   (save       (or (equal arg '(16)) (equal arg '(64))))
	   (noselect   (equal arg '(1)))
	   (new-window (if arg
			   (not (or (equal arg '(4)) (equal arg '(64))))
			 (and (not (custom/org-diary-in-entry))
			      (or custom/org-diary-visit-in-new-window
			          (> (window-width) 70))))))
       ;; Whether to initialize the diary entry
       (setq init
	     (not (or (file-exists-p entry)
		      (custom/org-diary-entry-unsaved-buffer time))))
       ;; Open entry
       (custom/org-diary-open entry noselect new-window)
       ;; Initialize
       (if init
	      (progn (custom/org-diary-init time)
		     (if save (save-buffer))))
       ;; Enable `org-diary-mode'
       (custom/org-diary-mode)
       ;; Go to end of buffer
       (end-of-buffer)))

(defun custom/org-diary-today (&optional arg)
  "Open the Org Diary entry for today, creating it if
it does not exist."
  (interactive)
  (custom/org-diary-visit (current-time) arg custom/org-diary-directory))

(defun custom/org-diary-jump (number)
  (interactive)
  (let ((custom/org-diary-visit-in-new-window (not (custom/org-diary-in-entry)))
	   (time-jump (time-add (custom/org-diary-entry-time) (days-to-time number))))
    (custom/org-diary-visit time-jump '(4))))

(defun custom/org-diary-prior ()
  (interactive)
  (custom/org-diary-jump -1))

(defun custom/org-diary-next ()
  (interactive)
  (custom/org-diary-jump 1))

(defun custom/org-diary-init (time)
  "Set up Org Diary entry."
  (interactive)
  (insert (concat "#+title:" (custom/org-diary-time-string-title time) "\n"))
  (insert "#+CREATED: ")
  (org-time-stamp-inactive '(16))
  (insert "\n\n\n"))

(defun custom/org-diary-insert-time (format)
  "Insert current time using the given FORMAT."
  (insert (format-time-string format (current-time))))

(defun custom/org-diary-insert-time-hhmm ()
  "Insert current time using the given FORMAT."
  (interactive)
  (custom/org-diary-insert-time "%H:%M"))

(defun custom/org-diary (&optional arg)
  "Org Diary entry point.

Activate when visiting files matching pattern.

Bindings:
- C-<up>   -> previous entry if it exists
- C-<down> -> next entry if it exists
- C-n      -> new entry"
  (interactive)
  (if (custom/org-diary-in-entry)
      (progn (custom/org-diary-mode 0)
	       (bury-buffer)
	       (ignore-errors (delete-window)))
    (progn (custom/org-diary-today arg)
	     (custom/org-diary-mode 1))))

(add-hook 'org-mode-hook (lambda () (if (custom/org-diary-in-entry) (custom/org-diary-mode))))

(global-set-key (kbd "C-c d") 'custom/org-diary)

(define-key org-mode-map (kbd "C-d")       'custom/org-diary-insert-time-hhmm)
(define-key org-mode-map (kbd "C-x w")     'custom/org-diary-resize-window)
(define-key org-mode-map (kbd "C-c t")     'custom/org-diary-today)
(define-key org-mode-map (kbd "C-<prior>") 'custom/org-diary-prior)
(define-key org-mode-map (kbd "C-<next>")  'custom/org-diary-next)

(provide 'org-diary)
;;; org-modern.el ends here
