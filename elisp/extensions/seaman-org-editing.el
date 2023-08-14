(defun seaman/org-undo ()
  (interactive)
  (if (org-babel-where-is-src-block-head)
      (let ((beg (point)))
	         (cond ((eq last-command 'seaman/org-return)    (undo 2))
                       ((eq last-command 'seaman/smart-comment) (undo 1))
	               (t                                       (undo 1)))
           (if (org-babel-where-is-src-block-head)
	             (save-excursion (goto-char (org-babel-where-is-src-block-head))
				     (next-line)
				     (setq hang (point))))
		 (if (= hang (point))
		     (progn (goto-char beg)
			    (beginning-of-line-text))))
    (undo 1)))

(define-key org-mode-map (kbd "C-/") #'seaman/org-undo)

(defun seaman/org-insert-item-respect-content ()
  (interactive)
  (let ((struct (org-list-struct))
	    (unfold (if (seaman/org-relative-line-list-folded) nil (point-marker))))
    (org-list-set-item-visibility (point-at-bol) struct 'folded)
    (save-excursion
      (beginning-of-visual-line)
      (kill-ring-save (point) (seaman/get-point 'beginning-of-line-text)))
    (end-of-visual-line)
    (org-return)
    (yank)
    (if unfold (save-excursion (goto-char unfold) (org-list-set-item-visibility (point-at-bol) struct 'subtree)))))

(defun seaman/org-heading-margin-post ()
  "Return margin between current heading and next."
  (if (org-current-level)
      (let ((pos (seaman/get-point 'seaman/org-goto-heading-bol))
	    (end-of-subtree (seaman/get-point 'seaman/org-goto-subtree-end))
	    (next-heading   (seaman/get-point 'seaman/org-goto-heading-next)))
	(if (not (and (= pos end-of-subtree) (seaman/org-relative-line-heading)))
	    (buffer-substring-no-properties end-of-subtree next-heading)
	  ""))
    (if (seaman/org-headings-follow)
	       (buffer-substring-no-properties (point) (seaman/get-point 'seaman/org-goto-heading-next))
      "")))

(defun seaman/org-heading-margin-delete-post ()
  "Delete newline after new headings created by
`respect-content' heading commands."
  (if (seaman/org-subtree-blank)
      (apply 'delete-region (seaman/org-get-subtree-region))))

(defun seaman/org-heading-margin-insert-previous ()
  "If the previous subtree is not empty,
insert a margin of 1 empty line."
  (let ((insert-margin (save-excursion (if (seaman/org-heading-first-child)
					          (seaman/org-goto-heading-previous)
					        (org-backward-heading-same-level 1))
				              (not (seaman/org-subtree-blank)))))
    (if insert-margin
      (progn (beginning-of-visual-line)
	            (org-return)
		    (beginning-of-line-text)))))

(defun seaman/org-insert-heading (command &optional margin)
  "Primitive for custom heading functions.

If cursor if at an Org Mode heading's
ellipsis, go to the `end-of-line' of the
heading's visual line.

If cursor lies on an Org Mode heading,
`seaman/org-show-minimum'.

If cursor is outside top level heading,
insert heading at point, without removing
any of the previous space.

If the previous subtree is not empty,
insert a margin of 1 empty line.
This is because Org Mode heading insertion
commands will automatically remove all [[:space:]]
until first preceding non-empty line.

If MARGIN is t:
- insert margin between content under parent heading and new one"
  (interactive)
  (if (seaman/org-at-ellipsis-h)         (progn (beginning-of-visual-line) (end-of-line)))
  (if (seaman/org-relative-line-heading) (seaman/org-show-minimum))
  ;; Insert heading
  (cond ((not (org-current-level)) (insert "* "))
        (t                         (funcall command)))
  ;; Insert margin
  (if margin (seaman/org-heading-margin-insert-previous))
  ;; Hide previous subtree
  (if (save-excursion (seaman/org-goto-heading-previous)
		      (seaman/org-relative-line-heading-folded))
      (seaman/org-hide-previous-subtree)))

(defun seaman/org-insert-subheading (orig-fun &optional arg)
  "Make `org-insert-subheading' ARG optional."
  (interactive)
  (let ((arg (or arg 0)))
    (funcall orig-fun arg)))

(advice-add 'org-insert-subheading :around #'seaman/org-insert-subheading)

(defun seaman/org-insert-heading-at-point ()
  (interactive)
  (seaman/org-insert-heading 'org-insert-heading (not (seaman/org-subtree-blank-up-to-point))))

(defun seaman/org-insert-subheading-at-point ()
  (interactive)
  (seaman/org-insert-heading 'org-insert-subheading (not (seaman/org-subtree-blank-up-to-point))))

(defun seaman/org-insert-heading-after-subtree ()
  "Insert heading after current subtree. As
`org-insert-heading-respect-content' does not
behave well with folded Org Mode headings, if
the previous heading is folded:
1. Unfold the heading
2. Create the new heading after its subtree
3. Fold it back"
  (let ((margin-post        (seaman/count-substrings "\n" (seaman/org-heading-margin-post)))
	(prev-same-level    (seaman/get-point 'beginning-of-visual-line))
	(prev-lower-level   (seaman/get-point 'seaman/org-goto-child-last))
	(folded-same-level  (seaman/org-relative-line-heading-folded))
	(folded-lower-level (save-excursion (seaman/org-goto-child-last)
                                            (seaman/org-relative-line-heading-folded))))

    ;; Go to current heading
    (seaman/org-goto-heading-current)

    ;; Unfold if necessary
    (if folded-same-level  (save-excursion (org-show-subtree)))
    (if folded-lower-level (save-excursion (seaman/org-goto-subtree-end) (org-show-subtree)))
    
    ;; Insert heading
    (cond ((not (org-current-level)) (insert "* "))
	        (t                         (progn (seaman/org-goto-heading-current) (org-insert-heading-respect-content))))
    (seaman/org-heading-margin-delete-post)

    ;; Insert margin with previous heading
    (seaman/org-heading-margin-insert-previous)
    
    ;; Fold back if necessary
    (if folded-same-level  (save-excursion (goto-char prev-same-level)  (outline-hide-subtree)))
    (if folded-lower-level (save-excursion (goto-char prev-lower-level) (outline-hide-subtree)))

    ;; Recover margin with following heading
    (if (> margin-post 1) (save-excursion (insert "\n")))))

(defun seaman/org-insert-subheading-after-subtree ()
  "`org-insert-subheading' respecting content."
  (interactive)
  (seaman/org-show-minimum)
  (if (seaman/org-heading-has-children)
      (progn (seaman/org-goto-child-last)
	           (seaman/org-insert-heading-after-subtree))
    (progn (seaman/org-insert-heading-after-subtree)
	         (org-do-demote))))

(defvar seaman/org-functions-at-ellipsis '(org-self-insert-command
					   seaman/kill-ring-mouse)
  "Functions whose behavior at Org Mode ellipses
will be advised by `seaman/org-edit-at-ellipsis'")

(defun seaman/org-edit-at-ellipsis (orig-fun &rest args)
  "Execute commands invoked at an Org Mode heading's
ellipsis in the first line under the heading."
  (if (seaman/org-at-ellipsis-h)
      (progn (beginning-of-visual-line)
	     (seaman/org-show-minimum)
	     (end-of-line)
	     (org-return)
	     (apply orig-fun args))
    (apply orig-fun args)))

(dolist (function seaman/org-functions-at-ellipsis)
  (advice-add function :around #'seaman/org-edit-at-ellipsis))

;; org-return
(defun seaman/org-return ()
  "Conditional `org-return'."
  (interactive)
  (cond ((seaman/org-relative-line-list-empty)          (progn (seaman/delete-line) (org-return)))
	     ((seaman/org-at-bol-list)                       (progn (beginning-of-visual-line) (org-return) (beginning-of-line-text)))
	     ((seaman/org-at-ellipsis-l)                     (seaman/org-insert-item-respect-content))
	     ((seaman/org-relative-line-paragraph)           (org-insert-item))
	     ((seaman/org-relative-line-list)                (org-meta-return))
	     ((and (seaman/org-after-list-or-indent) (bolp)) (org-return))
	     ((seaman/org-at-bol-heading)                    (save-excursion (beginning-of-visual-line) (org-return t)))
	     ((seaman/org-at-eol-heading)                    (progn (newline 2) (if (seaman/org-subtree-blank) (progn (newline) (previous-line)))))
	     ((seaman/org-at-ellipsis-h)                     (org-return))
	     (t                                              (org-return t))))

(define-key org-mode-map (kbd "<return>") #'seaman/org-return)

;; org-meta-return
(defun seaman/org-control-return ()
  (interactive)
  (cond ((seaman/org-relative-line-list-empty) (progn (org-meta-return) (next-line) (end-of-line)))
	    ((seaman/org-relative-line-heading)    (seaman/org-insert-heading-after-subtree))
	    ((seaman/org-relative-line-list)       (progn (end-of-line) (org-meta-return)))
	    ((seaman/org-relative-line-paragraph)  (seaman/org-paragraph-toggle))
	    (t                                     (seaman/org-insert-heading-after-subtree))))

(define-key org-mode-map (kbd "C-<return>") #'seaman/org-control-return)

(defun seaman/org-meta-return ()
  (interactive)
  (seaman/org-insert-subheading-after-subtree))

(define-key org-mode-map (kbd "M-<return>") #'seaman/org-meta-return)

(defun seaman/org-super-return ()
  (interactive)
  (cond ((or (seaman/org-relative-line-list)
	     (seaman/org-relative-line-paragraph)) (org-return t))
	(t                                         (seaman/org-insert-subheading-at-point))))

(define-key org-mode-map (kbd "S-<return>") #'seaman/org-super-return)

(define-key org-mode-map (kbd "M-S-<return>") #'seaman/org-insert-heading-at-point)

(define-key org-mode-map (kbd "C-S-<return>") 'org-insert-todo-heading)

(define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-subheading)

(defun seaman/org-delete-hungry ()
  "If the region starts at the beginning of an 
indented line and the cursor lies on an Org Mode
src block, delete the region and its indent plus 
one character."
  (interactive)
  (seaman/@delete-hungry (org-in-src-block-p)))

(defun seaman/org-nimble-delete-forward ()
  "Org Mode complement to `seaman/nimble-delete-forward'."
  (interactive)
  (cond ((and (seaman/org-at-ellipsis-h)
	           (seaman/org-relative-line-heading 1))  (progn (beginning-of-visual-line 2)
								 (beginning-of-line-text)
								 (delete-forward-char 1)))
	      (t (seaman/nimble-delete-forward))))

(define-key org-mode-map (kbd "<delete>") #'seaman/org-nimble-delete-forward)

(defun seaman/org-nimble-delete-backward ()
  "Org Mode complement to `seaman/nimble-delete-backward'."
  (interactive)
  (cond ((and (region-active-p)
	           (not (seaman/region-blank)))                 (seaman/org-delete-hungry))
	     ((or  (seaman/org-at-ellipsis-h)
		   (seaman/org-at-ellipsis-l))                  (progn (beginning-of-visual-line) (end-of-line) (delete-backward-char 1)))
	     ((and (or (seaman/org-relative-line-heading-empty)
		       (seaman/org-relative-line-list-empty))
		   (org-current-level))                         (delete-region (point) (seaman/get-point 'end-of-line 0)))
	     ((or  (seaman/org-relative-line-heading-empty)
		   (seaman/org-relative-line-list-empty))       (delete-region (point) (seaman/get-point 'beginning-of-visual-line)))
	     ((seaman/org-at-bol-list)                          (seaman/org-toggle-item))
        (t                                                 (seaman/nimble-delete-backward))))

(define-key org-mode-map (kbd "<backspace>") #'seaman/org-nimble-delete-backward)

(defun seaman/org-toggle-item ()
  (interactive)
  (let ((toggle-off (seaman/org-relative-line-list))
	     (indent     (+ 1 org-list-indent-offset))
	     (marker     (point)))
    (beginning-of-line-text)
    (delete-backward-char indent)
    (if toggle-off
	     (insert (make-string indent ?\s))
      (org-toggle-item 0))
    (goto-char marker)))

(defun seaman/org-indent-region (command &rest args)
  "Indent Org Mode region.

If the region spans Org Mode headings or items:
1. Extend region by pushing `region-beginning' to its
`beginning-of-visual-line'
2. Execute COMMAND
3. Restore the region to its previous limits, shifting
its limits to match shifts in the position of the
text it spans, such as when indenting with `org-metaright'
or outdenting with `org-metaleft'."
  (if (or (seaman/org-relative-line-heading) (seaman/org-relative-line-list))
      (let ((beg (region-beginning))
	         (end (region-end))
		 (pos (point)))
	
	        ;; Determine mark
	        (setq mark (if (= pos beg) end beg))
		;; Count lines in region
		(setq lines (count-screen-lines beg end))

		;; Get initial cursor position wrt bol
		(setq relative-pos-0 (- pos (seaman/get-point 'beginning-of-line)))
		;; Execute command
		(save-excursion (goto-char beg)
				(push-mark (seaman/get-point 'beginning-of-line))
		 		(goto-char end)
				(end-of-visual-line)
				(apply command args))
		;; Get aftermath cursor position
		(setq pos-1 (point))
		;; Calculate cursor displacement
		(setq disp (- pos-1 pos))
		
           ;; Get aftermath cursor position wrt bol
		(setq relative-pos-1 (- pos-1 (seaman/get-point 'beginning-of-line)))
		;; Calculate cursor displacement wrt bol
		(setq relative-disp (- relative-pos-1 relative-pos-0))

		;; Calculate mark shift
		(cond
		 ((seaman/org-at-heading beg)  (setq shift disp))
		 ((= mark beg)                 (setq shift relative-disp))
		 ((= mark end)                 (setq shift (* relative-disp lines))))

		;; Push mark
		(push-mark (+ mark shift)))
    (apply command args)))

(defun seaman/with-mark-active (&rest args)
  "Keep mark active after command. To be used as advice AFTER any
function that sets `deactivate-mark' to t."
  (setq deactivate-mark nil))

(advice-add 'org-metaright      :after #'seaman/with-mark-active)
(advice-add 'org-metaleft       :after #'seaman/with-mark-active)
(advice-add 'org-metaup         :after #'seaman/with-mark-active)
(advice-add 'org-metadown       :after #'seaman/with-mark-active)

(advice-add 'org-shiftmetaright :after #'seaman/with-mark-active)
(advice-add 'org-shiftmetaleft  :after #'seaman/with-mark-active)
(advice-add 'org-shiftmetaup    :after #'seaman/with-mark-active)
(advice-add 'org-shift-metadown :after #'seaman/with-mark-active)

;; Do not insert newline before Org Mode headings
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(provide 'seaman-extension-org-editing)
;;; seaman-org-editing.el ends here
