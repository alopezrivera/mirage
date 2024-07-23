(defun mirage/org-undo ()
  (interactive)
  (if (org-babel-where-is-src-block-head)
      (let ((beg (point)))
	         (cond ((eq last-command 'mirage/org-return)    (undo 2))
                       ((eq last-command 'mirage/smart-comment) (undo 1))
	               (t                                       (undo 1)))
           (if (org-babel-where-is-src-block-head)
	             (save-excursion (goto-char (org-babel-where-is-src-block-head))
				     (next-line)
				     (setq hang (point))))
		 (if (= hang (point))
		     (progn (goto-char beg)
			    (beginning-of-line-text))))
    (undo 1)))

(define-key org-mode-map (kbd "C-/") #'mirage/org-undo)

(defun mirage/org-insert-item-respect-content ()
  (interactive)
  (let ((struct (org-list-struct))
	    (unfold (if (mirage/org-relative-line-list-folded) nil (point-marker))))
    (org-list-set-item-visibility (point-at-bol) struct 'folded)
    (save-excursion
      (beginning-of-visual-line)
      (kill-ring-save (point) (mirage/get-point 'beginning-of-line-text)))
    (end-of-visual-line)
    (org-return)
    (yank)
    (if unfold (save-excursion (goto-char unfold) (org-list-set-item-visibility (point-at-bol) struct 'subtree)))))

(defun mirage/org-heading-margin-post ()
  "Return margin between current heading and next."
  (if (org-current-level)
      (let ((pos (mirage/get-point 'mirage/org-goto-heading-bol))
	    (end-of-subtree (mirage/get-point 'mirage/org-goto-subtree-end))
	    (next-heading   (mirage/get-point 'mirage/org-goto-heading-next)))
	(if (not (and (= pos end-of-subtree) (mirage/org-relative-line-heading)))
	    (buffer-substring-no-properties end-of-subtree next-heading)
	  ""))
    (if (mirage/org-headings-follow)
	       (buffer-substring-no-properties (point) (mirage/get-point 'mirage/org-goto-heading-next))
      "")))

(defun mirage/org-heading-margin-delete-post ()
  "Delete newline after new headings created by
`respect-content' heading commands."
  (if (mirage/org-subtree-blank)
      (apply 'delete-region (mirage/org-get-subtree-region))))

(defun mirage/org-heading-margin-insert-previous ()
  "If the previous subtree is not empty,
insert a margin of 1 empty line."
  (let ((insert-margin (save-excursion (if (mirage/org-heading-first-child)
					          (mirage/org-goto-heading-previous)
					        (org-backward-heading-same-level 1))
				              (not (mirage/org-subtree-blank)))))
    (if insert-margin
      (progn (beginning-of-visual-line)
	            (org-return)
		    (beginning-of-line-text)))))

(defun mirage/org-insert-heading (command &optional margin)
  "Primitive for custom heading functions.

If cursor if at an Org Mode heading's
ellipsis, go to the `end-of-line' of the
heading's visual line.

If cursor lies on an Org Mode heading,
`mirage/org-show-minimum'.

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
  (if (mirage/org-at-ellipsis-h)         (progn (beginning-of-visual-line) (end-of-line)))
  (if (mirage/org-relative-line-heading) (mirage/org-show-minimum))
  ;; Insert heading
  (cond ((not (org-current-level)) (insert "* "))
        (t                         (funcall command)))
  ;; Insert margin
  (if margin (mirage/org-heading-margin-insert-previous))
  ;; Hide previous subtree
  (if (save-excursion (mirage/org-goto-heading-previous)
		      (mirage/org-relative-line-heading-folded))
      (mirage/org-hide-previous-subtree)))

(defun mirage/org-insert-subheading (orig-fun &optional arg)
  "Make `org-insert-subheading' ARG optional."
  (interactive)
  (let ((arg (or arg 0)))
    (funcall orig-fun arg)))

(advice-add 'org-insert-subheading :around #'mirage/org-insert-subheading)

(defun mirage/org-insert-heading-at-point ()
  (interactive)
  (mirage/org-insert-heading 'org-insert-heading (not (mirage/org-subtree-blank-up-to-point))))

(defun mirage/org-insert-subheading-at-point ()
  (interactive)
  (mirage/org-insert-heading 'org-insert-subheading (not (mirage/org-subtree-blank-up-to-point))))

(defun mirage/org-insert-heading-after-subtree ()
  "Insert heading after current subtree. As
`org-insert-heading-respect-content' does not
behave well with folded Org Mode headings, if
the previous heading is folded:
1. Unfold the heading
2. Create the new heading after its subtree
3. Fold it back"
  (let ((margin-post        (mirage/count-substrings "\n" (mirage/org-heading-margin-post)))
	(prev-same-level    (mirage/get-point 'beginning-of-visual-line))
	(prev-lower-level   (mirage/get-point 'mirage/org-goto-child-last))
	(folded-same-level  (mirage/org-relative-line-heading-folded))
	(folded-lower-level (save-excursion (mirage/org-goto-child-last)
                                            (mirage/org-relative-line-heading-folded))))

    ;; Go to current heading
    (mirage/org-goto-heading-current)

    ;; Unfold if necessary
    (if folded-same-level  (save-excursion (org-show-subtree)))
    (if folded-lower-level (save-excursion (mirage/org-goto-subtree-end) (org-show-subtree)))
    
    ;; Insert heading
    (cond ((not (org-current-level)) (insert "* "))
	        (t                         (progn (mirage/org-goto-heading-current) (org-insert-heading-respect-content))))
    (mirage/org-heading-margin-delete-post)

    ;; Insert margin with previous heading
    (mirage/org-heading-margin-insert-previous)
    
    ;; Fold back if necessary
    (if folded-same-level  (save-excursion (goto-char prev-same-level)  (outline-hide-subtree)))
    (if folded-lower-level (save-excursion (goto-char prev-lower-level) (outline-hide-subtree)))

    ;; Recover margin with following heading
    (if (> margin-post 1) (save-excursion (insert "\n")))))

(defun mirage/org-insert-subheading-after-subtree ()
  "`org-insert-subheading' respecting content."
  (interactive)
  (mirage/org-show-minimum)
  (if (mirage/org-heading-has-children)
      (progn (mirage/org-goto-child-last)
	           (mirage/org-insert-heading-after-subtree))
    (progn (mirage/org-insert-heading-after-subtree)
	         (org-do-demote))))

(defvar mirage/org-functions-at-ellipsis '(org-self-insert-command
					   mirage/kill-ring-mouse)
  "Functions whose behavior at Org Mode ellipses
will be advised by `mirage/org-edit-at-ellipsis'")

(defun mirage/org-edit-at-ellipsis (orig-fun &rest args)
  "Execute commands invoked at an Org Mode heading's
ellipsis in the first line under the heading."
  (if (mirage/org-at-ellipsis-h)
      (progn (beginning-of-visual-line)
	     (mirage/org-show-minimum)
	     (end-of-line)
	     (org-return)
	     (apply orig-fun args))
    (apply orig-fun args)))

(dolist (function mirage/org-functions-at-ellipsis)
  (advice-add function :around #'mirage/org-edit-at-ellipsis))

;; org-return
(defun mirage/org-return ()
  "Conditional `org-return'."
  (interactive)
  (cond ((mirage/org-relative-line-list-empty)          (progn (mirage/delete-line) (org-return)))
	     ((mirage/org-at-bol-list)                       (progn (beginning-of-visual-line) (org-return) (beginning-of-line-text)))
	     ((mirage/org-at-ellipsis-l)                     (mirage/org-insert-item-respect-content))
	     ((mirage/org-relative-line-paragraph)           (org-insert-item))
	     ((mirage/org-relative-line-list)                (org-meta-return))
	     ((and (mirage/org-after-list-or-indent) (bolp)) (org-return))
	     ((mirage/org-at-bol-heading)                    (save-excursion (beginning-of-visual-line) (org-return t)))
	     ((mirage/org-at-eol-heading)                    (progn (newline 2) (if (mirage/org-subtree-blank) (progn (newline) (previous-line)))))
	     ((mirage/org-at-ellipsis-h)                     (org-return))
	     (t                                              (org-return t))))

(define-key org-mode-map (kbd "<return>") #'mirage/org-return)

;; org-meta-return
(defun mirage/org-control-return ()
  (interactive)
  (cond ((mirage/org-relative-line-list-empty) (progn (org-meta-return) (next-line) (end-of-line)))
	    ((mirage/org-relative-line-heading)    (mirage/org-insert-heading-after-subtree))
	    ((mirage/org-relative-line-list)       (progn (end-of-line) (org-meta-return)))
	    ((mirage/org-relative-line-paragraph)  (mirage/org-paragraph-toggle))
	    (t                                     (mirage/org-insert-heading-after-subtree))))

(define-key org-mode-map (kbd "C-<return>") #'mirage/org-control-return)

(defun mirage/org-meta-return ()
  (interactive)
  (mirage/org-insert-subheading-after-subtree))

(define-key org-mode-map (kbd "M-<return>") #'mirage/org-meta-return)

(defun mirage/org-super-return ()
  (interactive)
  (cond ((or (mirage/org-relative-line-list)
	     (mirage/org-relative-line-paragraph)) (org-return t))
	(t                                         (mirage/org-insert-subheading-at-point))))

(define-key org-mode-map (kbd "S-<return>") #'mirage/org-super-return)

(define-key org-mode-map (kbd "M-S-<return>") #'mirage/org-insert-heading-at-point)

(define-key org-mode-map (kbd "C-S-<return>") 'org-insert-todo-heading)

(define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-subheading)

(defun mirage/org-delete-hungry ()
  "If the region starts at the beginning of an 
indented line and the cursor lies on an Org Mode
src block, delete the region and its indent plus 
one character."
  (interactive)
  (mirage/@delete-hungry (org-in-src-block-p)))

(defun mirage/org-nimble-delete-forward ()
  "Org Mode complement to `mirage/nimble-delete-forward'."
  (interactive)
  (cond ((and (mirage/org-at-ellipsis-h)
	           (mirage/org-relative-line-heading 1))  (progn (beginning-of-visual-line 2)
								 (beginning-of-line-text)
								 (delete-forward-char 1)))
	      (t (mirage/nimble-delete-forward))))

(define-key org-mode-map (kbd "<delete>") #'mirage/org-nimble-delete-forward)

(defun mirage/org-nimble-delete-backward ()
  "Org Mode complement to `mirage/nimble-delete-backward'."
  (interactive)
  (cond ((and (region-active-p)
	           (not (mirage/region-blank)))                 (mirage/org-delete-hungry))
	     ((or  (mirage/org-at-ellipsis-h)
		   (mirage/org-at-ellipsis-l))                  (progn (beginning-of-visual-line) (end-of-line) (delete-backward-char 1)))
	     ((and (or (mirage/org-relative-line-heading-empty)
		       (mirage/org-relative-line-list-empty))
		   (org-current-level))                         (delete-region (point) (mirage/get-point 'end-of-line 0)))
	     ((or  (mirage/org-relative-line-heading-empty)
		   (mirage/org-relative-line-list-empty))       (delete-region (point) (mirage/get-point 'beginning-of-visual-line)))
	     ((mirage/org-at-bol-list)                          (mirage/org-toggle-item))
        (t                                                 (mirage/nimble-delete-backward))))

(define-key org-mode-map (kbd "<backspace>") #'mirage/org-nimble-delete-backward)

(defun mirage/org-toggle-item ()
  (interactive)
  (let ((toggle-off (mirage/org-relative-line-list))
	     (indent     (+ 1 org-list-indent-offset))
	     (marker     (point)))
    (beginning-of-line-text)
    (delete-backward-char indent)
    (if toggle-off
	     (insert (make-string indent ?\s))
      (org-toggle-item 0))
    (goto-char marker)))

(defun mirage/org-indent-region (command &rest args)
  "Indent Org Mode region.

If the region spans Org Mode headings or items:
1. Extend region by pushing `region-beginning' to its
`beginning-of-visual-line'
2. Execute COMMAND
3. Restore the region to its previous limits, shifting
its limits to match shifts in the position of the
text it spans, such as when indenting with `org-metaright'
or outdenting with `org-metaleft'."
  (if (or (mirage/org-relative-line-heading) (mirage/org-relative-line-list))
      (let ((beg (region-beginning))
	         (end (region-end))
		 (pos (point)))
	
	        ;; Determine mark
	        (setq mark (if (= pos beg) end beg))
		;; Count lines in region
		(setq lines (count-screen-lines beg end))

		;; Get initial cursor position wrt bol
		(setq relative-pos-0 (- pos (mirage/get-point 'beginning-of-line)))
		;; Execute command
		(save-excursion (goto-char beg)
				(push-mark (mirage/get-point 'beginning-of-line))
		 		(goto-char end)
				(end-of-visual-line)
				(apply command args))
		;; Get aftermath cursor position
		(setq pos-1 (point))
		;; Calculate cursor displacement
		(setq disp (- pos-1 pos))
		
           ;; Get aftermath cursor position wrt bol
		(setq relative-pos-1 (- pos-1 (mirage/get-point 'beginning-of-line)))
		;; Calculate cursor displacement wrt bol
		(setq relative-disp (- relative-pos-1 relative-pos-0))

		;; Calculate mark shift
		(cond
		 ((mirage/org-at-heading beg)  (setq shift disp))
		 ((= mark beg)                 (setq shift relative-disp))
		 ((= mark end)                 (setq shift (* relative-disp lines))))

		;; Push mark
		(push-mark (+ mark shift)))
    (apply command args)))

(defun mirage/with-mark-active (&rest args)
  "Keep mark active after command. To be used as advice AFTER any
function that sets `deactivate-mark' to t."
  (setq deactivate-mark nil))

(advice-add 'org-metaright      :after #'mirage/with-mark-active)
(advice-add 'org-metaleft       :after #'mirage/with-mark-active)
(advice-add 'org-metaup         :after #'mirage/with-mark-active)
(advice-add 'org-metadown       :after #'mirage/with-mark-active)

(advice-add 'org-shiftmetaright :after #'mirage/with-mark-active)
(advice-add 'org-shiftmetaleft  :after #'mirage/with-mark-active)
(advice-add 'org-shiftmetaup    :after #'mirage/with-mark-active)
(advice-add 'org-shift-metadown :after #'mirage/with-mark-active)

;; Do not insert newline before Org Mode headings
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(provide 'mirage-extension-org-editing)
;;; mirage-org-editing.el ends here
