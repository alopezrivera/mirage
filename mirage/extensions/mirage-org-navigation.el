(defun mirage/org-end ()
  "Conditional end in Org Mode.

Default: `mirage/end'

If `org-at-table-p', go to `org-table-end-of-field'."
  (cond ((and (region-active-p) (mirage/org-relative-line-heading-folded)) (end-of-line))
        ((org-at-table-p)                                                  (org-table-end-of-field 1))
	(t                                                                 (end-of-visual-line))))

(defvar mirage/org-double-end-timeout 0.4)

(defun mirage/org-double-end ()
  "Dynamic homing command with a timeout of `mirage/org-double-end-timeout' seconds.
- Single press: `mirage/org-home' 
- Double press: `beginning-of-visual-line'"
  (interactive)
  (let ((last-called (get this-command 'mirage/last-call-time)))
    (if (and (eq last-command this-command)	     
             (<= (time-to-seconds (time-since last-called)) mirage/org-double-end-timeout)
	         (not (org-at-table-p)))
        (progn (beginning-of-visual-line) (end-of-line))
      (mirage/org-end)))
  (put this-command 'mirage/last-call-time (current-time)))

(define-key org-mode-map (kbd "<end>") #'mirage/org-double-end)

(defun mirage/org-home ()
     "Conditional homing in Org Mode.

Default: `mirage/home'

If a multi-visual-line region is active and the cursor lies on a heading or
list item, home to `beginning-of-visual-line'.

If a region is active the cursor lies `mirage/org-at-ellipsis-h', home to
`beginning-of-visual-line'.

If the cursor lies `mirage/org-at-ellipsis-h' (no active region), home to
the `beginning-of-line-text' of the heading's visual line.

If the cursor lies on at heading or list, home to `beginning-of-line-text'.

If the cursor lies in a source code block, and the current line is a wrapped
visual line, home to `beginning-of-visual-line'.

If the cursor lies in a source code block, home `back-to-indentation'.

If `org-at-table-p', home to `org-table-beginning-of-field'."
   (interactive)
   (cond ((and (mirage/region-multiline-visual) (mirage/org-relative-line-heading-or-list))  (beginning-of-visual-line))
         ((and (region-active-p) (mirage/org-at-ellipsis-h))                                 (beginning-of-visual-line))
         ((mirage/org-at-ellipsis-h)                                                         (mirage/org-goto-heading-bol))
	     ((mirage/org-at-ellipsis-l)                                                         (mirage/org-goto-heading-bol))
	     ((mirage/relative-line-wrapped)                                                     (beginning-of-visual-line))
         ((mirage/org-relative-line-heading-or-list)                                         (beginning-of-line-text))
         ((org-in-src-block-p)                                                               (back-to-indentation))
	     ((org-at-table-p)                                                                   (org-table-beginning-of-field 1))
         (t                                                                                  (mirage/home))))

(defvar mirage/org-double-home-timeout 0.4)

(defun mirage/org-double-home ()
  "Dynamic homing command with a timeout of `mirage/org-double-home-timeout' seconds.
- Single press: `mirage/org-home' 
- Double press: `beginning-of-visual-line'"
  (interactive)
  (let ((last-called (get this-command 'mirage/last-call-time)))
    (if (and (eq last-command this-command)	     
             (<= (time-to-seconds (time-since last-called)) mirage/org-double-home-timeout)
	         (not (org-at-table-p)))
	    (beginning-of-line)
      (mirage/org-home)))
  (put this-command 'mirage/last-call-time (current-time)))

(define-key org-mode-map (kbd "<home>") #'mirage/org-double-home)

(defun mirage/org-goto-child-last ()
  (if (org-current-level)
      (progn (mirage/org-goto-subtree-end)
	     (mirage/org-goto-heading-current))))

(defun mirage/org-goto-subtree-end ()
  (mirage/org-goto-heading-current)
  (org-end-of-subtree)
  (if (mirage/org-relative-line-heading-folded) (end-of-visual-line)))

(defun mirage/org-goto-heading-bol ()
  (beginning-of-visual-line)
  (beginning-of-line-text))

(defun mirage/org-goto-heading-next ()
  (mirage/org-goto-heading-current)
  (let ((pos (mirage/get-point 'beginning-of-visual-line)))
       (org-forward-heading-same-level 1)
       (if (= pos (point))
	   (progn (mirage/org-goto-heading-parent)
		  (org-forward-heading-same-level 1)))))

(defun mirage/org-goto-heading-parent ()
  (let ((current (mirage/get-point 'beginning-of-visual-line)))
    (if (and (org-current-level)
	        (not (= 1 (org-current-level)))
		(= current (mirage/get-point 'beginning-of-visual-line)))
	   (outline-up-heading 1))))

(defun mirage/org-goto-heading-current ()
  (if (org-current-level) (outline-back-to-heading)))

(defun mirage/org-goto-heading-previous ()
  (mirage/org-goto-heading-current)
  (let ((current (mirage/get-point 'beginning-of-visual-line)))
    ;; go to previous same-level heading
    (org-backward-heading-same-level 1)
    ;; if there was no previous same-level heading, go to parent if not at top
    (if (= (point) current)
	(mirage/org-goto-heading-parent)
      ;; else, attempt going to last subheading of previous same-level heading
      (mirage/org-goto-child-last))))

(defmacro @mirage/org-dir-link-complete (name dir)
  `(defun ,(intern (concat "org-" name "-link-complete")) ()
     "Create an org-link target string to a file in org-`name'-link-complete."
     (concat ,name ":" (file-relative-name (read-file-name "File: " ,dir) ,dir))))

(defmacro @mirage/org-dir-link-follow (name dir)
  `(defun ,(intern (concat "org-" name "-link-follow")) (link)
     "Follow an org-link to a file in org-`name'-link-follow."
     (find-file (expand-file-name link ,dir))))

(defmacro @mirage/org-dir-link (name dir)
  `(progn (@mirage/org-dir-link-complete ,name ,dir)
          (@mirage/org-dir-link-follow   ,name ,dir)
          (org-link-set-parameters ,name
                                   :complete ',(intern (concat "org-" name "-link-complete"))
                                   :follow   ',(intern (concat "org-" name "-link-follow")))))

(provide 'mirage-extension-org-navigation)
;;; mirage-org-navigation.el ends here
