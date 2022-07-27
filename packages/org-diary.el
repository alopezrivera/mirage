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
  "Org Diary minor mode"
  :init-value nil
  :lighter " Diary"
  :group 'custom/org-diary-mode-group
  :keymap '()

  (let ((active (bound-and-true-p custom/org-diary-mode)))
    (when active
      (custom/org-diary-typeset))
    (when (not active)
      (custom/org-diary-undo-typesetting))))

(define-globalized-minor-mode custom/org-diary-global-minor-mode custom/org-diary-mode custom/org-diary-mode :group 'custom/org-diary-mode-group)

(defcustom custom/org-diary-directory           "/home/diary/"
  "Org Diary directory"
  :group 'custom/org-diary-mode-group
  :type 'boolean)

(defcustom custom/org-diary-time-format-file    "%d-%m-%Y"
  "Org Diary time format: file names"
  :group 'custom/org-diary-mode-group
  :type 'string)

(defcustom custom/org-diary-entry-regex         "^[0-9]\\{2\\}\\-[0-9]\\{2\\}\\-[0-9]\\{4\\}\\.org"
  "Regex query to identify Org Diary entries"
  :group 'custom/org-diary-mode-group
  :type 'string)

(defcustom custom/org-diary-time-format-title   "%d/%m/%Y"
  "Org Diary time format: entry titles"
  :group 'custom/org-diary-mode-group
  :type 'string)

(defcustom custom/org-diary-new-window-fraction 0.25
  "New Org Diary window width as a fraction of the frame width"
  :group 'custom/org-diary-mode-group
  :type 'float)

(defcustom custom/org-diary-min-window-width    65
  "Minimum width of an Org Diary window"
  :group 'custom/org-diary-mode-group
  :type 'integer)

(defcustom custom/org-diary-morph-window-width-factor 1.5
  "Multipled with the width of an Org Diary window in the current frame
to determine the width below which `org-diary' will not split the current
window to create one for the appropriate entry, but rather switch to it in
the current window and resize it if necessary"
  :group 'custom/org-diary-mode-group
  :type 'integer)

(defcustom custom/org-diary-visit-in-new-window t
  "Whether to open diary entries in new window"
  :group 'custom/org-diary-mode-group
  :type 'boolean)

(defcustom custom/org-diary-variable-pitch      nil
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

(defvar custom/org-diary-last-visited nil
  "Time of the last Org Diary entry being edited before exiting Org Diary.
Upon being called again, `org-diary' will open this entry so you can
resume your writing where you left off")

(defun custom/org-diary-file-format (&optional dir)
  "Org Diary file name format"
  (let ((dir  (or dir
		     (if (and custom/org-diary-navigate-in-current-dir buffer-file-name)
			 (file-name-directory buffer-file-name)
		       custom/org-diary-directory)))
	   (file custom/org-diary-time-format-file))
    (concat dir file ".org")))

(defun custom/org-diary-window-width ()
  "Width of an Org Diary window in the current frame"
  (max (* (frame-width) custom/org-diary-new-window-fraction)
       custom/org-diary-min-window-width))

(defun custom/org-diary-new-window ()
  "Create a window for an Org Diary entry or use the current one
if it is too narrow to split, and resize it."
  (if (> (window-total-width) (* custom/org-diary-morph-window-width-factor (custom/org-diary-window-width)))
      (progn (split-window-horizontally)
	         (windmove-right)))
  (if (not (ignore-errors (custom/org-diary-resize-window)))
      (delete-other-windows)))

(defun custom/org-diary-resize-window ()
  (interactive)
  (custom/window-resize-fraction custom/org-diary-new-window-fraction custom/org-diary-min-window-width))

(defun custom/org-diary-typeset ()
  "Typeset `org-diary' buffers"
  ;; variable pitch
  (if custom/org-diary-variable-pitch
      (variable-pitch-mode))
  ;; font overlays
  (custom/org-diary-font-lock-add)
  ;; pretty entities
  (if org-pretty-entities
      (org-toggle-pretty-entities)))

(defun custom/org-diary-undo-typesetting ()
  "Undo `org-diary' typesetting"
  (custom/org-diary-font-lock-remove)
  (if custom/org-diary-variable-pitch
      (variable-pitch-mode 0)))

(defface custom/org-diary-typeface-hhmm
  '((nil :foreground "#eb07b6" :inherit 'fixed-pitch))
  "Org Diary typeface for hh:mm time stamps"
  :group 'custom/org-diary-mode-group)

(defcustom custom/org-diary-keyword-hhmm '("[0-9]\\{2\\}:[0-9]\\{2\\}$" . 'custom/org-diary-typeface-hhmm)
  "Org Diary hh:mm typeface keyword"
  :group 'custom/org-diary-mode-group)

(defcustom custom/org-diary-keywords (list custom/org-diary-keyword-hhmm)
  "Org Diary font-lock keywords")

(defun custom/org-diary-font-lock-add ()
  (font-lock-add-keywords nil custom/org-diary-keywords)
  (font-lock-fontify-buffer))

(defun custom/org-diary-font-lock-remove ()
  (font-lock-remove-keywords nil custom/org-diary-keywords)
  (font-lock-fontify-buffer))

(defun custom/org-diary-parse-time (string)
  "Parse time string. Currently hardcoded to parse time
strings in the format `%d/%m/%Y'"
  (let ((dmy (cl-loop for n in (split-string string "/")
		            collect (string-to-number n))))
    (encode-time (list 0 0 0 (nth 0 dmy) (nth 1 dmy) (nth 2 dmy) nil nil nil))))

(defun custom/org-diary-time-string-file (time &optional dir)
  (format-time-string (custom/org-diary-file-format dir) time))

(defun custom/org-diary-time-string-title (time)
  (format-time-string custom/org-diary-time-format-title time))

(defun custom/org-diary-entry (&optional buffer)
  "Return t if BUFFER is an Org Diary entry"
  (let ((bfname (buffer-file-name (or buffer (current-buffer)))))
    (if bfname
	    (string-match-p custom/org-diary-entry-regex (file-name-nondirectory bfname)))))

(defun custom/org-diary-entry-file (&optional buffer)
  "Retrieve the file name of an Org Diary entry"
  (custom/org-diary-time-string-file (custom/org-diary-entry-time buffer)))

(defun custom/org-diary-entry-time (&optional buffer)
  "Retrieve the time of an Org Diary entry"
  (let ((title (custom/org-get-title-buffer buffer)))
      (custom/org-diary-parse-time title)))

(defun custom/org-diary-entry-unsaved-buffer (time)
  "Return t if the Org Diary entry for TIME exists
in an unsaved buffer"
  (let ((entry (file-name-nondirectory (custom/org-diary-time-string-file time))))
    (cl-loop for buffer in (buffer-list)
	         if (and (buffer-name buffer)
			 (string-match entry (buffer-name buffer)))
		    return t
             finally return nil)))

(defun custom/org-diary-window ()
  (cl-loop for buffer in (buffer-list)
	       if (custom/org-diary-entry buffer)
	          return (get-buffer-window buffer)
           finally return nil))

(defun custom/org-diary-init (time)
  "Set up Org Diary entry"
  (interactive)
  (insert (concat "#+title:" (custom/org-diary-time-string-title time) "\n"))
  (insert "#+CREATED: ")
  (org-time-stamp-inactive '(16))
  (insert "\n\n\n\n- "))

(defun custom/org-diary-insert-time (format)
  "Insert current time using the given FORMAT"
  (insert (format-time-string format (current-time))))

(defun custom/org-diary-insert-time-hhmm ()
  "Insert current time in HH:MM format"
  (interactive)
  (custom/org-diary-insert-time "%H:%M"))

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

ARG is the `current-prefix-arg' of a function wrapping this one, and which passes its
`current-prefix-arg' to this function.
-             '(0):  noselect
- C-u         '(4):  visit in current buffer
- C-u C-u     '(16): save new entry after initialiation
- C-u C-u C-u '(64): visit in current buffer and save new entry after initialization

DIR is the directory in which to look for the org-diary entry corresponding to TIME."
  (let ((entry      (custom/org-diary-time-string-file time dir))
	   (save       (or (equal arg '(16)) (equal arg '(64))))
	   (noselect   (equal arg '(1)))
	   (new-window (if arg
			   (not (or (equal arg '(4)) (equal arg '(64))))
			 (and (not (custom/org-diary-entry))
			      (or custom/org-diary-visit-in-new-window
			          (> (window-width) (* custom/org-diary-morph-window-width-factor (custom/org-diary-window-width))))))))
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

(defun custom/org-diary-today ()
  "Open the Org Diary entry for today, creating it if
it does not exist"
  (interactive)
  (custom/org-diary-visit (current-time) current-prefix-arg custom/org-diary-directory))

(defun custom/org-diary-jump (number)
  (interactive)
  (let ((custom/org-diary-visit-in-new-window (not (custom/org-diary-entry)))
	   (time-jump (time-add (custom/org-diary-entry-time) (days-to-time number))))
    (custom/org-diary-visit time-jump '(4))))

(defun custom/org-diary-prior ()
  (interactive)
  (custom/org-diary-jump -1))

(defun custom/org-diary-next ()
  (interactive)
  (custom/org-diary-jump 1))

(defun custom/org-diary-revisit ()
  "Determine whether to revisit the `custom/org-diary-last-visited' entry"
  (if custom/org-diary-last-visited
      (let ((entry (custom/org-diary-time-string-file custom/org-diary-last-visited custom/org-diary-directory)))
	   (custom/find-buffer-by-file-name entry))))

(defun custom/org-diary-exit ()
  (setq custom/org-diary-last-visited (custom/org-diary-entry-time (current-buffer)))
  (custom/org-diary-mode 0))

(defun custom/org-diary ()
  "Org Diary entry and exit point. If preceded by `C-u', prompt
for a date to visit using the Emacs calendar."
  (interactive)
  (if (equal current-prefix-arg '(4))
      (let ((time (org-read-date nil 'to-time nil "")))
           (custom/org-diary-visit time nil custom/org-diary-directory))
    (if (custom/org-diary-entry)
        (progn (custom/org-diary-exit)
	         (bury-buffer)
	         (ignore-errors (delete-window)))
      (progn (if (custom/org-diary-window)
	           (select-window (custom/org-diary-window))
	         (let ((time (if (custom/org-diary-revisit) custom/org-diary-last-visited (current-time))))
                  (custom/org-diary-visit time nil custom/org-diary-directory))
	       (custom/org-diary-mode 1))))))

(add-hook 'org-mode-hook (lambda () (if (custom/org-diary-entry) (custom/org-diary-mode))))

(global-set-key (kbd "C-c d") 'custom/org-diary)

(defvar custom/org-diary-bindings '(("C-d"       . custom/org-diary-insert-time-hhmm)
                                    ("C-x w"     . custom/org-diary-resize-window)
                                    ("C-c t"     . custom/org-diary-today)
                                    ("C-<prior>" . custom/org-diary-prior)
                                    ("C-<next>"  . custom/org-diary-next))
  "Org Diary bindings")

(cl-loop for binding in custom/org-diary-bindings
         collect (let ((k (car binding))
                       (c (cdr binding)))
                   (define-key custom/org-diary-mode-map (kbd k) c)))

(provide 'org-diary)
;;; org-modern.el ends here
