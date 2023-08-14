(defun seaman/org-end ()
  "Conditional end in Org Mode.

Default: `seaman/end'

If `org-at-table-p', go to `org-table-end-of-field'."
  (cond ((and (region-active-p) (seaman/org-relative-line-heading-folded)) (end-of-line))
        ((org-at-table-p)                                                  (org-table-end-of-field 1))
	(t                                                                 (end-of-visual-line))))

(defvar seaman/org-double-end-timeout 0.4)

(defun seaman/org-double-end ()
  "Dynamic homing command with a timeout of `seaman/org-double-end-timeout' seconds.
- Single press: `seaman/org-home' 
- Double press: `beginning-of-visual-line'"
  (interactive)
  (let ((last-called (get this-command 'seaman/last-call-time)))
    (if (and (eq last-command this-command)	     
             (<= (time-to-seconds (time-since last-called)) seaman/org-double-end-timeout)
	         (not (org-at-table-p)))
        (progn (beginning-of-visual-line) (end-of-line))
      (seaman/org-end)))
  (put this-command 'seaman/last-call-time (current-time)))

(define-key org-mode-map (kbd "<end>") #'seaman/org-double-end)

(defun seaman/org-home ()
     "Conditional homing in Org Mode.

Default: `seaman/home'

If a multi-visual-line region is active and the cursor lies on a heading or
list item, home to `beginning-of-visual-line'.

If a region is active the cursor lies `seaman/org-at-ellipsis-h', home to
`beginning-of-visual-line'.

If the cursor lies `seaman/org-at-ellipsis-h' (no active region), home to
the `beginning-of-line-text' of the heading's visual line.

If the cursor lies on at heading or list, home to `beginning-of-line-text'.

If the cursor lies in a source code block, and the current line is a wrapped
visual line, home to `beginning-of-visual-line'.

If the cursor lies in a source code block, home `back-to-indentation'.

If `org-at-table-p', home to `org-table-beginning-of-field'."
   (interactive)
   (cond ((and (seaman/region-multiline-visual) (seaman/org-relative-line-heading-or-list))  (beginning-of-visual-line))
         ((and (region-active-p) (seaman/org-at-ellipsis-h))                                 (beginning-of-visual-line))
         ((seaman/org-at-ellipsis-h)                                                         (seaman/org-goto-heading-bol))
	     ((seaman/org-at-ellipsis-l)                                                         (seaman/org-goto-heading-bol))
	     ((seaman/relative-line-wrapped)                                                     (beginning-of-visual-line))
         ((seaman/org-relative-line-heading-or-list)                                         (beginning-of-line-text))
         ((org-in-src-block-p)                                                               (back-to-indentation))
	     ((org-at-table-p)                                                                   (org-table-beginning-of-field 1))
         (t                                                                                  (seaman/home))))

(defvar seaman/org-double-home-timeout 0.4)

(defun seaman/org-double-home ()
  "Dynamic homing command with a timeout of `seaman/org-double-home-timeout' seconds.
- Single press: `seaman/org-home' 
- Double press: `beginning-of-visual-line'"
  (interactive)
  (let ((last-called (get this-command 'seaman/last-call-time)))
    (if (and (eq last-command this-command)	     
             (<= (time-to-seconds (time-since last-called)) seaman/org-double-home-timeout)
	         (not (org-at-table-p)))
	    (beginning-of-line)
      (seaman/org-home)))
  (put this-command 'seaman/last-call-time (current-time)))

(define-key org-mode-map (kbd "<home>") #'seaman/org-double-home)

(defun seaman/org-goto-child-last ()
  (if (org-current-level)
      (progn (seaman/org-goto-subtree-end)
	     (seaman/org-goto-heading-current))))

(defun seaman/org-goto-subtree-end ()
  (seaman/org-goto-heading-current)
  (org-end-of-subtree)
  (if (seaman/org-relative-line-heading-folded) (end-of-visual-line)))

(defun seaman/org-goto-heading-bol ()
  (beginning-of-visual-line)
  (beginning-of-line-text))

(defun seaman/org-goto-heading-next ()
  (seaman/org-goto-heading-current)
  (let ((pos (seaman/get-point 'beginning-of-visual-line)))
       (org-forward-heading-same-level 1)
       (if (= pos (point))
	   (progn (seaman/org-goto-heading-parent)
		  (org-forward-heading-same-level 1)))))

(defun seaman/org-goto-heading-parent ()
  (let ((current (seaman/get-point 'beginning-of-visual-line)))
    (if (and (org-current-level)
	        (not (= 1 (org-current-level)))
		(= current (seaman/get-point 'beginning-of-visual-line)))
	   (outline-up-heading 1))))

(defun seaman/org-goto-heading-current ()
  (if (org-current-level) (outline-back-to-heading)))

(defun seaman/org-goto-heading-previous ()
  (seaman/org-goto-heading-current)
  (let ((current (seaman/get-point 'beginning-of-visual-line)))
    ;; go to previous same-level heading
    (org-backward-heading-same-level 1)
    ;; if there was no previous same-level heading, go to parent if not at top
    (if (= (point) current)
	(seaman/org-goto-heading-parent)
      ;; else, attempt going to last subheading of previous same-level heading
      (seaman/org-goto-child-last))))

(defmacro @seaman/org-dir-link-complete (name dir)
  `(defun ,(intern (concat "org-" name "-link-complete")) ()
     "Create an org-link target string to a file in org-`name'-link-complete."
     (concat ,name ":" (file-relative-name (read-file-name "File: " ,dir) ,dir))))

(defmacro @seaman/org-dir-link-follow (name dir)
  `(defun ,(intern (concat "org-" name "-link-follow")) (link)
     "Follow an org-link to a file in org-`name'-link-follow."
     (find-file (expand-file-name link ,dir))))

(defmacro @seaman/org-dir-link (name dir)
  `(progn (@seaman/org-dir-link-complete ,name ,dir)
          (@seaman/org-dir-link-follow   ,name ,dir)
          (org-link-set-parameters ,name
                                   :complete ',(intern (concat "org-" name "-link-complete"))
                                   :follow   ',(intern (concat "org-" name "-link-follow")))))

(provide 'shapes-extension-org-navigation)
;;; shapes-org-navigation.el ends here
