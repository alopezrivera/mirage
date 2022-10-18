;;; -*- lexical-binding: t; -*-

(defun custom/org-end ()
  "Conditional end in Org Mode.

Default: `custom/end'

If `org-at-table-p', go to `org-table-end-of-field'."
  (cond ((and (region-active-p) (custom/org-relative-line-heading-folded)) (end-of-line))
        ((org-at-table-p)                                                  (org-table-end-of-field 1))
	(t                                                                 (end-of-visual-line))))

(defvar custom/org-double-end-timeout 0.4)

(defun custom/org-double-end ()
  "Dynamic homing command with a timeout of `custom/org-double-end-timeout' seconds.
- Single press: `custom/org-home' 
- Double press: `beginning-of-visual-line'"
  (interactive)
  (let ((last-called (get this-command 'custom/last-call-time)))
    (if (and (eq last-command this-command)	     
             (<= (time-to-seconds (time-since last-called)) custom/org-double-end-timeout)
	         (not (org-at-table-p)))
        (progn (beginning-of-visual-line) (end-of-line))
      (custom/org-end)))
  (put this-command 'custom/last-call-time (current-time)))

(define-key org-mode-map (kbd "<end>") 'custom/org-double-end)

(defun custom/org-home ()
     "Conditional homing in Org Mode.

Default: `custom/home'

If a multi-visual-line region is active and the cursor lies on a heading or
list item, home to `beginning-of-visual-line'.

If a region is active the cursor lies `custom/org-at-ellipsis-h', home to
`beginning-of-visual-line'.

If the cursor lies `custom/org-at-ellipsis-h' (no active region), home to
the `beginning-of-line-text' of the heading's visual line.

If the cursor lies on at heading or list, home to `beginning-of-line-text'.

If the cursor lies in a source code block, and the current line is a wrapped
visual line, home to `beginning-of-visual-line'.

If the cursor lies in a source code block, home `back-to-indentation'.

If `org-at-table-p', home to `org-table-beginning-of-field'."
   (interactive)
   (cond ((and (custom/region-multiline-visual) (custom/org-relative-line-heading-or-list))  (beginning-of-visual-line))
         ((and (region-active-p) (custom/org-at-ellipsis-h))                                 (beginning-of-visual-line))
         ((custom/org-at-ellipsis-h)                                                         (custom/org-goto-heading-bol))
	     ((custom/org-at-ellipsis-l)                                                         (custom/org-goto-heading-bol))
	     ((custom/relative-line-wrapped)                                                     (beginning-of-visual-line))
         ((custom/org-relative-line-heading-or-list)                                         (beginning-of-line-text))
         ((org-in-src-block-p)                                                               (back-to-indentation))
	     ((org-at-table-p)                                                                   (org-table-beginning-of-field 1))
         (t                                                                                  (custom/home))))

(defvar custom/org-double-home-timeout 0.4)

(defun custom/org-double-home ()
  "Dynamic homing command with a timeout of `custom/org-double-home-timeout' seconds.
- Single press: `custom/org-home' 
- Double press: `beginning-of-visual-line'"
  (interactive)
  (let ((last-called (get this-command 'custom/last-call-time)))
    (if (and (eq last-command this-command)	     
             (<= (time-to-seconds (time-since last-called)) custom/org-double-home-timeout)
	         (not (org-at-table-p)))
	    (beginning-of-line)
      (custom/org-home)))
  (put this-command 'custom/last-call-time (current-time)))

(define-key org-mode-map (kbd "<home>") 'custom/org-double-home)

(defun custom/org-goto-child-last ()
  (if (org-current-level)
      (progn (custom/org-goto-subtree-end)
	     (custom/org-goto-heading-current))))

(defun custom/org-goto-subtree-end ()
  (custom/org-goto-heading-current)
  (org-end-of-subtree)
  (if (custom/org-relative-line-heading-folded) (end-of-visual-line)))

(defun custom/org-goto-heading-bol ()
  (beginning-of-visual-line)
  (beginning-of-line-text))

(defun custom/org-goto-heading-next ()
  (custom/org-goto-heading-current)
  (let ((pos (custom/get-point 'beginning-of-visual-line)))
       (org-forward-heading-same-level 1)
       (if (= pos (point))
	   (progn (custom/org-goto-heading-parent)
		  (org-forward-heading-same-level 1)))))

(defun custom/org-goto-heading-parent ()
  (let ((current (custom/get-point 'beginning-of-visual-line)))
    (if (and (org-current-level)
	        (not (= 1 (org-current-level)))
		(= current (custom/get-point 'beginning-of-visual-line)))
	   (outline-up-heading 1))))

(defun custom/org-goto-heading-current ()
  (if (org-current-level) (outline-back-to-heading)))

(defun custom/org-goto-heading-previous ()
  (custom/org-goto-heading-current)
  (let ((current (custom/get-point 'beginning-of-visual-line)))
    ;; go to previous same-level heading
    (org-backward-heading-same-level 1)
    ;; if there was no previous same-level heading, go to parent if not at top
    (if (= (point) current)
	    (custom/org-goto-heading-parent)
      ;; else, attempt going to last subheading of previous same-level heading
      (custom/org-goto-child-last))))

(provide 'shapes-extension-org-navigation)
;;; shapes-org-navigation.el ends here
