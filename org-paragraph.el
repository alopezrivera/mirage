(defun custom/org-paragraph-toggle ()
  (interactive)
  (if (not (custom/org-relative-line-list))
      (progn ;; If the paragraph is indented,
	           ;; assume it will have a visual
	           ;; indent as the one created by this
             ;; function, and revert it before
             ;; turning paragraph into item.
	           (if (custom/relative-line-indented)
	               (progn (setq back (+ (point) org-list-indent-offset))
		              (beginning-of-line-text)
			      (insert (make-string org-list-indent-offset ?\s))
			      (goto-char back)))
		   ;; Record whether cursor is at `bolp'
		   ;; or `custom/at-indent'
		   (setq bol (or (bolp) (custom/at-indent)))
             ;; Turn into item
	           (org-toggle-item (point))
		   ;; If cursor was at either `bolp'
		   ;; or `custom/at-indent', move to
		   ;; `beginning-of-line-text'
		   (if bol (beginning-of-line-text)))
    (progn ;; Drop off
           (org-toggle-item (point))
	         ;; Ensure cursor remains at
		 ;; `beginning-of-line-text'
		 (if (bolp) (beginning-of-line-text))
		 (if (custom/relative-line-indented)
		     (progn (setq back (- (point) org-list-indent-offset))
			    (beginning-of-line-text)
			    (delete-backward-char org-list-indent-offset)
			    (goto-char back))))))

(defvar custom/org-paragraph-ignore-errors t
  "Ignore errors in `org-paragraph' calls.")

(defun custom/org-paragraph (command &rest args)
  "Org Mode hitch-hiking paragraphs."
  (if (and (not (custom/org-relative-line-heading))
           (not (custom/org-relative-line-list))
           (custom/org-relative-line-list -1))
      (progn
        ;; Hitch ride
	  (custom/org-paragraph-toggle)
	  ;; Execute command
	  (if custom/org-paragraph-ignore-errors
	      (ignore-errors (apply command args))
	    (apply command args))
	  ;; Drop off
	  (custom/org-paragraph-toggle))))

(defun custom/org-paragraph-indent ()
  (interactive)
  (custom/org-paragraph 'org-indent-item))

(defun custom/org-paragraph-outdent ()
  (interactive)
  (custom/org-paragraph 'org-outdent-item))

(provide 'org-paragraph)
