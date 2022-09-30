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

(defgroup bitacora-mode-group nil
  "Group for customization"
  :prefix "bitacora-")

(define-minor-mode bitacora-mode
  "Bitácora minor mode"
  :init-value nil
  :lighter " Diary"
  :group 'bitacora-mode-group
  :keymap '()

  (let ((active (bound-and-true-p bitacora-mode)))
    (when active
      (bitacora-typeset))
    (when (not active)
      (bitacora-undo-typesetting))))

(define-globalized-minor-mode bitacora-global-minor-mode bitacora-mode bitacora-mode :group 'bitacora-mode-group)

(defcustom bitacora-directory         "/home/diary/"
  "Bitácora directory"
  :group 'bitacora-mode-group
  :type 'boolean)

(defcustom bitacora-time-format-file  "%d-%m-%Y"
  "Bitácora time format: file names"
  :group 'bitacora-mode-group
  :type 'string)

(defcustom bitacora-entry-regex       "^[0-9]\\{2\\}\\-[0-9]\\{2\\}\\-[0-9]\\{4\\}\\.org"
  "Regex query to identify Bitácora entries"
  :group 'bitacora-mode-group
  :type 'string)

(defcustom bitacora-time-format-title "%d/%m/%Y"
  "Bitácora time format: entry titles"
  :group 'bitacora-mode-group
  :type 'string)

(defcustom bitacora-new-window-fraction 0.225
  "New Bitácora window width as a fraction of the frame width"
  :group 'bitacora-mode-group
  :type 'float)

(defcustom bitacora-min-window-width 65
  "Minimum width of an Bitácora window"
  :group 'bitacora-mode-group
  :type 'integer)

(defcustom bitacora-morph-window-width-factor 1.5
  "Multipled with the width of an Bitácora window in the current frame
to determine the width below which `bitacora' will not split the current
window to create one for the appropriate entry, but rather switch to it in
the current window and resize it if necessary"
  :group 'bitacora-mode-group
  :type 'integer)

(defcustom bitacora-visit-in-new-window t
  "Whether to open diary entries in new window"
  :group 'bitacora-mode-group
  :type 'boolean)

(defcustom bitacora-variable-pitch nil
  "Whether to activate `variable-pitch-mode' in Bitácora entries"
  :group 'bitacora-mode-group
  :type 'boolean)

(defcustom bitacora-navigate-in-current-dir t
  "If the current buffer is an `bitacora' buffer, this variable
determines whether `bitacora-prior' and `bitacora-next' will
search (or create) the prior or next `bitacora' entry in the directory
of the current buffer as opposed to in `bitacora-directory'.
Setting this variable to t is useful to navigate directories with
notes in `bitacora' format"
  :group 'bitacora-mode-group
  :type 'boolean)

(defvar bitacora-last-visited nil
  "Time of the last Bitácora entry being edited before exiting Bitácora.
Upon being called again, `bitacora' will open this entry so you can
resume your writing where you left off")

(defun bitacora--org-get-title-buffer (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (nth 1
	   (assoc "TITLE"
		  (org-element-map (org-element-parse-buffer 'greater-element)
		      '(keyword)
		    #'bitacora--get-keyword-key-value))))))

(defun bitacora--window-resize-fraction (fr &optional min)
  "Resize window to a fraction of the frame width."
  (interactive)
  (let ((width (max (if min min 0) (truncate (* fr (frame-width))))))
    (window-resize nil (- width (window-width)) t)))

(defun bitacora--get-keyword-key-value (kwd)
   (let ((data (cadr kwd)))
     (list (plist-get data :key)
           (plist-get data :value))))

(defun bitacora--find-buffer-by-file-name (file)
  (cl-loop for buffer in (buffer-list)
	        if (string-equal (buffer-name buffer) (file-name-nondirectory file))
	           return buffer
		finally return nil))

(defun bitacora-window-width ()
  "Width of an Bitácora window in the current frame"
  (max (* (frame-width) bitacora-new-window-fraction)
       bitacora-min-window-width))

(defun bitacora-new-window ()
  "Create a window for an Bitácora entry or use the current one
if it is too narrow to split, and resize it."
  (if (> (window-total-width) (* bitacora-morph-window-width-factor (bitacora-window-width)))
      (progn (split-window-horizontally)
	         (windmove-right)))
  (if (not (ignore-errors (bitacora-resize-window)))
      (delete-other-windows)))

(defun bitacora-resize-window ()
  (interactive)
  (bitacora--window-resize-fraction bitacora-new-window-fraction bitacora-min-window-width))

(defun bitacora-typeset ()
  "Typeset `bitacora' buffers"
  ;; variable pitch
  (if bitacora-variable-pitch
      (variable-pitch-mode))
  ;; font overlays
  (bitacora-font-lock-add)
  ;; pretty entities
  (if org-pretty-entities
      (org-toggle-pretty-entities)))

(defun bitacora-undo-typesetting ()
  "Undo `bitacora' typesetting"
  (bitacora-font-lock-remove)
  (if bitacora-variable-pitch
      (variable-pitch-mode 0)))

(defface bitacora-typeface-hhmm
  '((nil :foreground "#eb07b6" :inherit 'fixed-pitch))
  "Bitácora typeface for hh:mm time stamps"
  :group 'bitacora-mode-group)

(defcustom bitacora-keyword-hhmm '("[0-9]\\{2\\}:[0-9]\\{2\\}$" . 'bitacora-typeface-hhmm)
  "Bitácora hh:mm typeface keyword"
  :group 'bitacora-mode-group)

(defcustom bitacora-keywords (list bitacora-keyword-hhmm)
  "Bitácora font-lock keywords")

(defun bitacora-font-lock-add ()
  (font-lock-add-keywords nil bitacora-keywords)
  (font-lock-fontify-buffer))

(defun bitacora-font-lock-remove ()
  (font-lock-remove-keywords nil bitacora-keywords)
  (font-lock-fontify-buffer))

(defun bitacora-file-format (&optional dir)
  "Bitácora file name format"
  (let ((dir (or dir
		     (if (and bitacora-navigate-in-current-dir buffer-file-name)
			 (file-name-directory buffer-file-name)
		       bitacora-directory)))
	   (file bitacora-time-format-file))
    (concat dir file ".org")))

(defun bitacora-parse-time (string)
  "Parse time string. Currently hardcoded to parse time
strings in the format `%d/%m/%Y'"
  (let ((dmy (cl-loop for n in (split-string string "/")
		            collect (string-to-number n))))
    (encode-time (list 0 0 0 (nth 0 dmy) (nth 1 dmy) (nth 2 dmy) nil nil nil))))

(defun bitacora-time-string-file (time &optional dir)
  (format-time-string (bitacora-file-format dir) time))

(defun bitacora-time-string-title (time)
  (format-time-string bitacora-time-format-title time))

(defun bitacora-entry (&optional buffer)
  "Return t if BUFFER is an Bitácora entry"
  (let ((bfname (buffer-file-name (or buffer (current-buffer)))))
    (if bfname
	    (string-match-p bitacora-entry-regex (file-name-nondirectory bfname)))))

(defun bitacora-entry-file (&optional buffer)
  "Retrieve the file name of an Bitácora entry"
  (bitacora-time-string-file (bitacora-entry-time buffer)))

(defun bitacora-entry-time (&optional buffer)
  "Retrieve the time of an Bitácora entry"
  (let ((title (bitacora--org-get-title-buffer buffer)))
      (bitacora-parse-time title)))

(defun bitacora-entry-unsaved-buffer (time)
  "Return t if the Bitácora entry for TIME exists
in an unsaved buffer"
  (let ((entry (file-name-nondirectory (bitacora-time-string-file time))))
    (cl-loop for buffer in (buffer-list)
	         if (and (buffer-name buffer)
			 (string-match entry (buffer-name buffer)))
		    return t
             finally return nil)))

(defun bitacora-window ()
  (cl-loop for buffer in (buffer-list)
	       if (bitacora-entry buffer)
	          return (get-buffer-window buffer)
           finally return nil))

(defun bitacora-init (time)
  "Set up Bitácora entry"
  (interactive)
  (insert (concat "#+title:" (bitacora-time-string-title time) "\n"))
  (insert "#+CREATED: ")
  (org-time-stamp-inactive '(16))
  (insert "\n\n\n\n- "))

(defun bitacora-insert-time (format)
  "Insert current time using the given FORMAT"
  (insert (format-time-string format (current-time))))

(defun bitacora-insert-time-hhmm ()
  "Insert current time in HH:MM format"
  (interactive)
  (bitacora-insert-time "%H:%M"))

(defun bitacora-browse ()
  "Org Agenda-like list of diary entries.
Options:
- org-recenter -> show diary at point in side by side window
- enter -> jump to diary entry at point
- quit -> quit and return to previous window config, buffer and visibility
  - org-agenda -> save current window config, visibility"
  (interactive))

(defun bitacora-open (entry &optional noselect new-window)
  "Open an Bitácora diary.

If a buffer for the entry exists, and the buffer is being displayed in a window,
switch to that window; otherwise, switch to that buffer.

- NOSELECT:   open entry file without selecting it
- NEW-WINDOW: open entry in new window"
  (setq entry-buffer (bitacora--find-buffer-by-file-name entry))
  (setq entry-window (if entry-buffer
			    (get-buffer-window entry-buffer)
		          nil))
  (cond (noselect                      (find-file-noselect entry))
	   (entry-window                  (select-window entry-window))
	   ((and entry-buffer new-window) (progn (bitacora-new-window) (switch-to-buffer entry-buffer)))
	   (new-window                    (progn (bitacora-new-window) (find-file        entry)))
	   (t                             (find-file entry))))

(defun bitacora-visit (time &optional arg dir)
  "Open the Bitácora entry corresponding to the specified time, and initialize it if necessary.

ARG is the `current-prefix-arg' of a function wrapping this one, and which passes its
`current-prefix-arg' to this function.
-             '(0):  noselect
- C-u         '(4):  visit in current buffer
- C-u C-u     '(16): save new entry after initialiation
- C-u C-u C-u '(64): visit in current buffer and save new entry after initialization

DIR is the directory in which to look for the bitacora entry corresponding to TIME."
  (let ((entry      (bitacora-time-string-file time dir))
	   (save       (or (equal arg '(16)) (equal arg '(64))))
	   (noselect   (equal arg '(1)))
	   (new-window (if arg
			   (not (or (equal arg '(4)) (equal arg '(64))))
			 (and (not (bitacora-entry))
			      (or bitacora-visit-in-new-window
			          (> (window-width) (* bitacora-morph-window-width-factor (bitacora-window-width))))))))
       ;; Whether to initialize the diary entry
       (setq init
	     (not (or (file-exists-p entry)
		      (bitacora-entry-unsaved-buffer time))))
       ;; Open entry
       (bitacora-open entry noselect new-window)
       ;; Initialize
       (if init
	      (progn (bitacora-init time)
		     (if save (save-buffer))))
       ;; Enable `bitacora-mode'
       (bitacora-mode)
       ;; Go to end of buffer
       (end-of-buffer)))

(defun bitacora-today ()
  "Open the Bitácora entry for today, creating it if
it does not exist"
  (interactive)
  (bitacora-visit (current-time) current-prefix-arg bitacora-directory))

(defun bitacora-jump (number)
  (interactive)
  (let ((bitacora-visit-in-new-window (not (bitacora-entry)))
	   (time-jump (time-add (bitacora-entry-time) (days-to-time number))))
    (bitacora-visit time-jump '(4))))

(defun bitacora-prior ()
  (interactive)
  (bitacora-jump -1))

(defun bitacora-next ()
  (interactive)
  (bitacora-jump 1))

(defun bitacora-revisit ()
  "Determine whether to revisit the `bitacora-last-visited' entry"
  (if bitacora-last-visited
      (let ((entry (bitacora-time-string-file bitacora-last-visited bitacora-directory)))
	   (bitacora--find-buffer-by-file-name entry))))

(defun bitacora-exit ()
  (setq bitacora-last-visited (bitacora-entry-time (current-buffer)))
  (bitacora-mode 0))

(defvar bitacora-register ""
  "Bitácora window configuration register")

(defun bitacora ()
  "Bitácora entry and exit point. If preceded by `C-u', prompt
for a date to visit using the Emacs calendar."
  (interactive)
  (if (equal current-prefix-arg '(4))
      (let ((time (org-read-date nil 'to-time nil "")))
           (window-configuration-to-register bitacora-register)
           (bitacora-visit time nil bitacora-directory))
    (if (bitacora-entry)
        (progn (bitacora-exit)
	         (bury-buffer)
               (jump-to-register bitacora-register))
      (progn (if (bitacora-window)
	           (select-window (bitacora-window))
	       (let ((time (if (bitacora-revisit) bitacora-last-visited (current-time))))
                  (window-configuration-to-register bitacora-register)
                  (bitacora-visit time nil bitacora-directory))
	       (bitacora-mode 1))))))

(add-hook 'org-mode-hook (lambda () (if (bitacora-entry) (bitacora-mode))))

(global-set-key (kbd "C-c d") 'bitacora)

(defvar bitacora-bindings '(("C-d"       . bitacora-insert-time-hhmm)
                            ("C-x w"     . bitacora-resize-window)
                            ("C-c t"     . bitacora-today)
                            ("C-<prior>" . bitacora-prior)
                            ("C-<next>"  . bitacora-next))
  "Bitácora bindings")

(cl-loop for binding in bitacora-bindings
         collect (let ((k (car binding))
                       (c (cdr binding)))
                   (define-key bitacora-mode-map (kbd k) c)))

(provide 'bitacora)
;;; org-modern.el ends here
