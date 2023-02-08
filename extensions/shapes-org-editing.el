;;; -*- lexical-binding: t; -*-

(defun custom/org-undo ()
  (interactive)
  (if (org-babel-where-is-src-block-head)
      (let ((beg (point)))
	         (cond ((eq last-command 'custom/org-return)    (undo 2))
                 ((eq last-command 'custom/smart-comment) (undo 1))
	               (t                                       (undo 1)))
           (if (org-babel-where-is-src-block-head)
	             (save-excursion (goto-char (org-babel-where-is-src-block-head))
				     (next-line)
				     (setq hang (point))))
		 (if (= hang (point))
		     (progn (goto-char beg)
			    (beginning-of-line-text))))
    (undo 1)))

(define-key org-mode-map (kbd "C-/") 'custom/org-undo)

(defun custom/org-insert-item-respect-content ()
  (interactive)
  (let ((struct (org-list-struct))
	    (unfold (if (custom/org-relative-line-list-folded) nil (point-marker))))
    (org-list-set-item-visibility (point-at-bol) struct 'folded)
    (save-excursion
      (beginning-of-visual-line)
      (kill-ring-save (point) (custom/get-point 'beginning-of-line-text)))
    (end-of-visual-line)
    (org-return)
    (yank)
    (if unfold (save-excursion (goto-char unfold) (org-list-set-item-visibility (point-at-bol) struct 'subtree)))))

(defun custom/org-heading-margin-post ()
  "Return margin between current heading and next."
  (if (org-current-level)
      (let ((pos            (custom/get-point 'custom/org-goto-heading-bol))
	           (end-of-subtree (custom/get-point 'custom/org-goto-subtree-end))
		   (next-heading   (custom/get-point 'custom/org-goto-heading-next)))
	          (if (not (and (= pos end-of-subtree) (custom/org-relative-line-heading)))
		      (buffer-substring-no-properties end-of-subtree next-heading)
		    ""))
    (if (custom/org-headings-follow)
	       (buffer-substring-no-properties (point) (custom/get-point 'custom/org-goto-heading-next))
      "")))

(defun custom/org-heading-margin-delete-post ()
  "Delete newline after new headings created by
`respect-content' heading commands."
  (if (custom/org-subtree-blank)
      (apply 'delete-region (custom/org-get-subtree-region))))

(defun custom/org-heading-margin-insert-previous ()
  "If the previous subtree is not empty,
insert a margin of 1 empty line."
  (let ((insert-margin (save-excursion (if (custom/org-heading-first-child)
					          (custom/org-goto-heading-previous)
					        (org-backward-heading-same-level 1))
				              (not (custom/org-subtree-blank)))))
    (if insert-margin
      (progn (beginning-of-visual-line)
	            (org-return)
		    (beginning-of-line-text)))))

(defun custom/org-insert-heading (command &optional margin)
  "Primitive for custom heading functions.

If cursor if at an Org Mode heading's
ellipsis, go to the `end-of-line' of the
heading's visual line.

If cursor lies on an Org Mode heading,
`custom/org-show-minimum'.

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
  (if (custom/org-at-ellipsis-h)         (progn (beginning-of-visual-line) (end-of-line)))
  (if (custom/org-relative-line-heading) (custom/org-show-minimum))
  ;; Insert heading
  (cond ((not (org-current-level)) (insert "* "))
        (t                         (funcall command)))
  ;; Insert margin
  (if margin (custom/org-heading-margin-insert-previous))
  ;; Hide previous subtree
  (if (save-excursion (custom/org-goto-heading-previous)
		      (custom/org-relative-line-heading-folded))
      (custom/org-hide-previous-subtree)))

(defun custom/org-insert-subheading (orig-fun &optional arg)
  "Make `org-insert-subheading' ARG optional."
  (interactive)
  (let ((arg (or arg 0)))
    (funcall orig-fun arg)))

(advice-add 'org-insert-subheading :around #'custom/org-insert-subheading)

(defun custom/org-insert-heading-at-point ()
  (interactive)
  (custom/org-insert-heading 'org-insert-heading (not (custom/org-subtree-blank-up-to-point))))

(defun custom/org-insert-subheading-at-point ()
  (interactive)
  (custom/org-insert-heading 'org-insert-subheading (not (custom/org-subtree-blank-up-to-point))))

(defun custom/org-insert-heading-after-subtree ()
  "Insert heading after current subtree. As
`org-insert-heading-respect-content' does not
behave well with folded Org Mode headings, if
the previous heading is folded:
1. Unfold the heading
2. Create the new heading after its subtree
3. Fold it back"
  (let ((margin-post        (custom/count-substrings "\n" (custom/org-heading-margin-post)))
	(prev-same-level    (custom/get-point 'beginning-of-visual-line))
	(prev-lower-level   (custom/get-point 'custom/org-goto-child-last))
	(folded-same-level  (custom/org-relative-line-heading-folded))
	(folded-lower-level (save-excursion (custom/org-goto-child-last)
                                            (custom/org-relative-line-heading-folded))))

    ;; Go to current heading
    (custom/org-goto-heading-current)

    ;; Unfold if necessary
    (if folded-same-level  (save-excursion (org-show-subtree)))
    (if folded-lower-level (save-excursion (custom/org-goto-subtree-end) (org-show-subtree)))
    
    ;; Insert heading
    (cond ((not (org-current-level)) (insert "* "))
	        (t                         (progn (custom/org-goto-heading-current) (org-insert-heading-respect-content))))
    (custom/org-heading-margin-delete-post)

    ;; Insert margin with previous heading
    (custom/org-heading-margin-insert-previous)
    
    ;; Fold back if necessary
    (if folded-same-level  (save-excursion (goto-char prev-same-level)  (outline-hide-subtree)))
    (if folded-lower-level (save-excursion (goto-char prev-lower-level) (outline-hide-subtree)))

    ;; Recover margin with following heading
    (if (> margin-post 1) (save-excursion (insert "\n")))))

(defun custom/org-insert-subheading-after-subtree ()
  "`org-insert-subheading' respecting content."
  (interactive)
  (custom/org-show-minimum)
  (if (custom/org-heading-has-children)
      (progn (custom/org-goto-child-last)
	           (custom/org-insert-heading-after-subtree))
    (progn (custom/org-insert-heading-after-subtree)
	         (org-do-demote))))

(defvar custom/org-functions-at-ellipsis '(org-self-insert-command
					   custom/kill-ring-mouse)
  "Functions whose behavior at Org Mode ellipses
will be advised by `custom/org-edit-at-ellipsis'")

(defun custom/org-edit-at-ellipsis (orig-fun &rest args)
  "Execute commands invoked at an Org Mode heading's
ellipsis in the first line under the heading."
  (if (custom/org-at-ellipsis-h)
      (progn (beginning-of-visual-line)
	     (custom/org-show-minimum)
	     (end-of-line)
	     (org-return)
	     (apply orig-fun args))
    (apply orig-fun args)))

(dolist (function custom/org-functions-at-ellipsis)
  (advice-add function :around #'custom/org-edit-at-ellipsis))

;; org-return
(defun custom/org-return ()
  "Conditional `org-return'."
  (interactive)
  (cond ((custom/org-relative-line-list-empty)          (progn (custom/delete-line) (org-return)))
	     ((custom/org-at-bol-list)                       (progn (beginning-of-visual-line) (org-return) (beginning-of-line-text)))
	     ((custom/org-at-ellipsis-l)                     (custom/org-insert-item-respect-content))
	     ((custom/org-relative-line-paragraph)           (org-insert-item))
	     ((custom/org-relative-line-list)                (org-meta-return))
	     ((and (custom/org-after-list-or-indent) (bolp)) (org-return))
	     ((custom/org-at-bol-heading)                    (save-excursion (beginning-of-visual-line) (org-return t)))
	     ((custom/org-at-eol-heading)                    (progn (newline 2) (if (custom/org-subtree-blank) (progn (newline) (previous-line)))))
	     ((custom/org-at-ellipsis-h)                     (org-return))
	     (t                                              (org-return t))))

(define-key org-mode-map (kbd "<return>") 'custom/org-return)

;; org-meta-return
(defun custom/org-control-return ()
  (interactive)
  (cond ((custom/org-relative-line-list-empty) (progn (org-meta-return) (next-line) (end-of-line)))
	    ((custom/org-relative-line-heading)    (custom/org-insert-heading-after-subtree))
	    ((custom/org-relative-line-list)       (progn (end-of-line) (org-meta-return)))
	    ((custom/org-relative-line-paragraph)  (custom/org-paragraph-toggle))
	    (t                                     (custom/org-insert-heading-after-subtree))))

(define-key org-mode-map (kbd "C-<return>") #'custom/org-control-return)

(defun custom/org-meta-return ()
  (interactive)
  (custom/org-insert-subheading-after-subtree))

(define-key org-mode-map (kbd "M-<return>") 'custom/org-meta-return)

(defun custom/org-super-return ()
  (interactive)
  (cond ((or (custom/org-relative-line-list)
	     (custom/org-relative-line-paragraph)) (org-return t))
	(t                                         (custom/org-insert-subheading-at-point))))

(define-key org-mode-map (kbd "S-<return>") 'custom/org-super-return)

(define-key org-mode-map (kbd "M-S-<return>") 'custom/org-insert-heading-at-point)

(define-key org-mode-map (kbd "C-S-<return>") 'org-insert-todo-heading)

(define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-subheading)

(defun custom/org-delete-hungry ()
  "If the region starts at the beginning of an 
indented line and the cursor lies on an Org Mode
src block, delete the region and its indent plus 
one character."
  (interactive)
  (custom/@delete-hungry (org-in-src-block-p)))

(defun custom/org-nimble-delete-forward ()
  "Org Mode complement to `custom/nimble-delete-forward'."
  (interactive)
  (cond ((and (custom/org-at-ellipsis-h)
	           (custom/org-relative-line-heading 1))  (progn (beginning-of-visual-line 2)
								 (beginning-of-line-text)
								 (delete-forward-char 1)))
	      (t (custom/nimble-delete-forward))))

(define-key org-mode-map (kbd "<delete>") 'custom/org-nimble-delete-forward)

(defun custom/org-nimble-delete-backward ()
  "Org Mode complement to `custom/nimble-delete-backward'."
  (interactive)
  (cond ((and (region-active-p)
	           (not (custom/region-blank)))                 (custom/org-delete-hungry))
	     ((or  (custom/org-at-ellipsis-h)
		   (custom/org-at-ellipsis-l))                  (progn (beginning-of-visual-line) (end-of-line) (delete-backward-char 1)))
	     ((and (or (custom/org-relative-line-heading-empty)
		       (custom/org-relative-line-list-empty))
		   (org-current-level))                         (delete-region (point) (custom/get-point 'end-of-line 0)))
	     ((or  (custom/org-relative-line-heading-empty)
		   (custom/org-relative-line-list-empty))       (delete-region (point) (custom/get-point 'beginning-of-visual-line)))
	     ((custom/org-at-bol-list)                          (custom/org-toggle-item))
        (t                                                 (custom/nimble-delete-backward))))

(define-key org-mode-map (kbd "<backspace>") 'custom/org-nimble-delete-backward)

(defun custom/org-toggle-item ()
  (interactive)
  (let ((toggle-off (custom/org-relative-line-list))
	     (indent     (+ 1 org-list-indent-offset))
	     (marker     (point)))
    (beginning-of-line-text)
    (delete-backward-char indent)
    (if toggle-off
	     (insert (make-string indent ?\s))
      (org-toggle-item 0))
    (goto-char marker)))

(defun custom/org-indent-region (command &rest args)
  "Indent Org Mode region.

If the region spans Org Mode headings or items:
1. Extend region by pushing `region-beginning' to its
`beginning-of-visual-line'
2. Execute COMMAND
3. Restore the region to its previous limits, shifting
its limits to match shifts in the position of the
text it spans, such as when indenting with `org-metaright'
or outdenting with `org-metaleft'."
  (if (or (custom/org-relative-line-heading) (custom/org-relative-line-list))
      (let ((beg (region-beginning))
	         (end (region-end))
		 (pos (point)))
	
	        ;; Determine mark
	        (setq mark (if (= pos beg) end beg))
		;; Count lines in region
		(setq lines (count-screen-lines beg end))

		;; Get initial cursor position wrt bol
		(setq relative-pos-0 (- pos (custom/get-point 'beginning-of-line)))
		;; Execute command
		(save-excursion (goto-char beg)
				(push-mark (custom/get-point 'beginning-of-line))
		 		(goto-char end)
				(end-of-visual-line)
				(apply command args))
		;; Get aftermath cursor position
		(setq pos-1 (point))
		;; Calculate cursor displacement
		(setq disp (- pos-1 pos))
		
           ;; Get aftermath cursor position wrt bol
		(setq relative-pos-1 (- pos-1 (custom/get-point 'beginning-of-line)))
		;; Calculate cursor displacement wrt bol
		(setq relative-disp (- relative-pos-1 relative-pos-0))

		;; Calculate mark shift
		(cond
		 ((custom/org-at-heading beg)  (setq shift disp))
		 ((= mark beg)                 (setq shift relative-disp))
		 ((= mark end)                 (setq shift (* relative-disp lines))))

		;; Push mark
		(push-mark (+ mark shift)))
    (apply command args)))

(defun custom/with-mark-active (&rest args)
  "Keep mark active after command. To be used as advice AFTER any
function that sets `deactivate-mark' to t."
  (setq deactivate-mark nil))

(advice-add 'org-metaright      :after #'custom/with-mark-active)
(advice-add 'org-metaleft       :after #'custom/with-mark-active)
(advice-add 'org-metaup         :after #'custom/with-mark-active)
(advice-add 'org-metadown       :after #'custom/with-mark-active)

(advice-add 'org-shiftmetaright :after #'custom/with-mark-active)
(advice-add 'org-shiftmetaleft  :after #'custom/with-mark-active)
(advice-add 'org-shiftmetaup    :after #'custom/with-mark-active)
(advice-add 'org-shift-metadown :after #'custom/with-mark-active)

;; Do not insert newline before Org Mode headings
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(provide 'shapes-extension-org-editing)
;;; shapes-org-editing.el ends here
