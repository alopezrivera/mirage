;;; -*- lexical-binding: t; -*-

;; `org-in-src-block-p' gives false positives as of Org Mode 9.5.3. For
;; this reason, determine if cursor in src block with the more reliable
;; `org-babel-where-is-src-block-head'
(advice-add 'org-in-src-block-p :override 'org-babel-where-is-src-block-head)

(defun custom/org-at-ellipsis (&optional position)
  (or (custom/org-at-ellipsis-h position) (custom/org-at-ellipsis-l position)))

(defun custom/org-at-ellipsis-l (&optional position)
  (and (custom/org-relative-line-list-folded) (custom/at-point 'end-of-visual-line)))

(defun custom/org-at-ellipsis-h (&optional position) 
  (and (custom/org-relative-line-heading-folded) (custom/at-point 'end-of-visual-line)))

(defun custom/org-at-keyword (&optional number)
  (custom/relative-line-regex "^#+.*$" number))

(defun custom/org-at-heading (&optional point)
  (let ((point (or point (point))))
    (save-excursion (goto-char point) (custom/org-relative-line-heading))))

(defun custom/org-at-bol-list () 
  (and (custom/org-relative-line-list) (custom/at-point 'beginning-of-line-text)))

(defun custom/org-at-bol-heading () 
  (and (custom/org-relative-line-heading) (custom/at-point 'custom/org-goto-heading-bol)))

(defun custom/org-at-eol-heading ()
  (and (custom/org-relative-line-heading) (eolp) (not (custom/org-at-ellipsis-h)) (not (custom/org-relative-line-heading-empty))))

(defun custom/org-after-list-or-indent ()
  (or (custom/org-relative-line-list -1) (custom/relative-line-indented -1)))

(defun custom/org-relative-line-list (&optional number)
  (custom/relative-line (lambda () (progn (beginning-of-line-text) (org-at-item-p)))  number))

(defun custom/org-relative-line-heading (&optional number)
  (custom/relative-line 'org-at-heading-p number))

(defun custom/org-relative-line-paragraph (&optional number)
  "Determine whether the current line -or the NUMBER'th line relative to it
is an indented paragraph."
  (let ((number (or number 0)))
    (and (not (custom/org-relative-line-heading number))
	       (not (custom/org-relative-line-list    number))
	       (not (org-in-src-block-p))
	       (custom/relative-line-indented number)
	       (or  (custom/org-relative-line-list      (- number 1))
		    (custom/org-relative-line-paragraph (- number 1))))))

(defun custom/org-relative-line-list-empty (&optional number)
  (and (custom/org-relative-line-list)
       (or (custom/relative-line-regex "^[[:blank:]]*[-+*]\\{1\\}[[:blank:]]+$" number)
	         (custom/relative-line-regex "^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]+$" number))))

(defun custom/org-relative-line-list-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded list item."
  (custom/relative-line (lambda () (and (org-at-item-p) (invisible-p (point-at-eol)))) number))

(defun custom/org-relative-line-heading-empty (&optional number)
  (custom/relative-line (lambda () (beginning-of-line-text) (org-point-at-end-of-empty-headline)) number))

(defun custom/org-relative-line-heading-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded heading."
  (custom/relative-line (lambda () (and (org-at-heading-p) (invisible-p (point-at-eol)))) number))

(defun custom/org-relative-line-heading-or-list (&optional number)
  (custom/relative-line 'org-at-heading-or-item-p number))

(defun custom/org-subtree-blank ()
  "Return t if the current subtree consists of
a `custom/region-blank'."
  (interactive)
  (apply #'custom/region-blank (custom/org-subtree-region)))

(defun custom/org-subtree-empty ()
  (interactive)
  (string-equal "" (custom/org-subtree-content)))

(defun custom/org-headings-follow ()
  (let ((pos (custom/get-point 'beginning-of-visual-line)))
    (save-excursion (custom/org-goto-heading-next)
		           (and (not (= pos (point))) (custom/org-relative-line-heading)))))

(defun custom/org-headings-precede ()
  (let ((pos (custom/get-point 'beginning-of-visual-line)))
    (save-excursion (custom/org-goto-heading-previous)
		          (and (not (= pos (point))) (custom/org-relative-line-heading)))))

(defun custom/org-subtree-blank-up-to-point ()
  (interactive)
  (let ((heading-eol (save-excursion (custom/org-goto-heading-current) (end-of-line) (point))))
    (custom/region-blank heading-eol (point))))

(defun custom/org-heading-first-child ()
  (save-excursion
    (custom/org-goto-heading-current)
    (let ((pos (custom/get-point 'beginning-of-visual-line)))
      (org-backward-heading-same-level 1)
      (= pos (custom/get-point 'beginning-of-visual-line)))))

(defun custom/org-heading-has-children ()
  (interactive)
  (save-excursion (org-goto-first-child)))

(defun custom/org-subtree-region (&optional element)
  "Retrieve the beginning and end of the current subtree."
  (if (org-element--cache-active-p)
      (let* ((heading (org-element-lineage
                       (or element (org-element-at-point))
                       '(headline) t))
	     (head (org-element-property :begin heading))
	     (next (org-element-property :end   heading)))
	  (if (and heading next)
	      (progn (save-excursion (goto-char head)
				     (beginning-of-line 2)
				     (setq beg (point)))
		     (save-excursion (goto-char next)
				     (beginning-of-line)
				     (setq end (max beg (point))))
		     (list beg end))))))

(defun custom/org-subtree-content ()
  "Retrieve the content of the current subtree."
  (setq content (apply #'buffer-substring-no-properties (custom/org-subtree-region))))

(defun custom/org-get-title-file (file)
  (with-current-buffer (find-file-noselect file)
       (custom/org-get-title-buffer)))

(defun custom/org-get-title-buffer (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (nth 1
	   (assoc "TITLE"
		  (org-element-map (org-element-parse-buffer 'greater-element)
		      '(keyword)
		    #'custom/get-keyword-key-value))))))

(defvar-local custom/org-outline-state nil
  "Place for saving org outline state before reverting the buffer.")

(put 'custom/org-outline-state 'permanent-local t)

(defun custom/org-set-outline-overlay-data (data)
  "Create visibility overlays for all positions in DATA.
DATA should have been made by `org-outline-overlay-data'."
  (org-with-wide-buffer
   (org-show-all)
   (dolist (c data) (org-flag-region (car c) (cdr c) t 'outline))))

(defun custom/org-restore-outline-state ()
  "Save org outline state in `custom/org-outline-state'.
It can be recovered afterwards with `custom/org-recover-outline-state'."
  (when custom/org-outline-state
    (custom/org-set-outline-overlay-data custom/org-outline-state)
    (setq custom/org-outline-state nil)))

(defun custom/org-outline-overlay-data (&optional use-markers)
  "Return a list of the locations of all outline overlays.
These are overlays with the `invisible' property value `outline'.
The return value is a list of cons cells, with start and stop
positions for each overlay.
If USE-MARKERS is set, return the positions as markers."
  (let (beg end)
    (org-with-wide-buffer
     (delq nil
       (mapcar (lambda (o)
             (when (eq (overlay-get o 'invisible) 'outline)
               (setq beg (overlay-start o)
                     end (overlay-end o))
               (and beg end (> end beg)
                (if use-markers
                (cons (copy-marker beg)
                      (copy-marker end t))
                  (cons beg end)))))
           (overlays-in (point-min) (point-max)))))))

(defun custom/org-save-outline-state ()
  "Save org outline state in `custom/org-outline-state'.
It can be recovered afterwards with `custom/org-recover-outline-state'."
  (setq custom/org-outline-state (custom/org-outline-overlay-data t)))

(defun custom/org-install-save-outline-state ()
  "Configure org to preserve the outline state at revert-buffer."
  (add-hook 'before-revert-hook #'custom/org-save-outline-state nil t)
  (add-hook 'after-revert-hook #'custom/org-restore-outline-state nil t))

(add-hook 'org-mode-hook #'custom/org-install-save-outline-state)

(defun custom/org-hide-previous-subtree ()
  "Cycle previous Org Mode heading."
  (save-excursion (custom/org-goto-heading-previous)
		        (outline-hide-subtree)))

(defun custom/org-show (orig-fun &rest args)
  (if (custom/org-at-ellipsis)
      (progn (custom/org-goto-heading-bol) (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'org-show-subtree :around #'custom/org-show)

(advice-add 'org-show-children :around #'custom/org-show)

(defun custom/org-show-minimum ()
  (if (or (custom/org-relative-line-list-folded)
	        (custom/org-relative-line-heading-folded))
      (progn (if (custom/org-at-ellipsis)
		       (progn (beginning-of-visual-line) (end-of-line)))
	           (org-show-entry)
	           (if (custom/org-heading-has-children) (org-show-children)))))

(defun custom/org-undo ()
  (interactive)
  (if (org-babel-where-is-src-block-head)
      (progn (setq beg (point))
	           (cond ((eq last-command 'custom/org-return)    (undo 2))
                   ((eq last-command 'custom/smart-comment) (undo 1))
	                 (t                                       (undo 1)))
	           (save-excursion (goto-char (org-babel-where-is-src-block-head))
				   (next-line)
				   (setq hang (point)))
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
      (apply 'delete-region (custom/org-subtree-region))))

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
  (let ((margin-post        (custom/regex-match-count "\n" (custom/org-heading-margin-post)))
	      (prev-same-level    (custom/get-point 'beginning-of-visual-line))
	      (prev-lower-level   (custom/get-point 'custom/org-goto-child-last))
	      (folded-same-level  (custom/org-relative-line-heading-folded))
	      (folded-lower-level (save-excursion (custom/org-goto-child-last) (custom/org-relative-line-heading-folded))))

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

(define-key org-mode-map (kbd "<deletechar>") 'custom/org-nimble-delete-forward)

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

(defun custom/org-cycle (orig-fun &rest args)
  "Conditional `org-cycle'.

Default: `org-cycle'

If cursor lies at `end-of-visual-line' of folded heading or list,
move cursor to `end-of-line' of the current visual line and then
call `org-cycle'.

If cursor lies at a paragraph directly under a list item and not
indented at the level of the previous list item, indent the paragraph."
  (interactive)
  (if (or (custom/org-relative-line-list-folded) (custom/org-relative-line-heading-folded))
      (if (= (point) (custom/get-point 'end-of-visual-line))
	  (progn (beginning-of-visual-line)
		 (end-of-line)
		 (apply orig-fun args))
	(apply orig-fun args))
    (if (and (org-in-src-block-p) (not (custom/org-at-keyword)))
	      (org-indent-line)
      (apply orig-fun args))))

(advice-add 'org-cycle :around #'custom/org-cycle)

(defun custom/org-mode (orig-fun &rest args)
  (if (custom/in-mode "org-mode")
      (progn (custom/org-save-outline-state)
	         (apply orig-fun args)
		 (custom/org-restore-outline-state))
    (apply orig-fun args)))

(advice-add 'org-mode :around #'custom/org-mode)

(defface custom/variable-pitch-marker
  '((nil :inherit 'fixed-pitch))
  "List marker typeface.")

(defface custom/variable-pitch-indent
  '((nil :inherit 'fixed-pitch :invisible t))
  "Indent typeface.")

(defvar custom/variable-pitch-keywords '(("^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]\\{1\\}" 0 'custom/variable-pitch-marker)
					     ("^[[:blank:]]*[-+]\\{1\\}[[:blank:]]\\{1\\}"         0 'custom/variable-pitch-marker)
					     ("^[[:blank:]]+"                                      0 'custom/variable-pitch-indent))
  "Variable pitch font-lock keywords.")

(font-lock-add-keywords 'org-mode custom/variable-pitch-keywords 'append)

;; continuous numbering of Org Mode equations
(defun org-renumber-environment (orig-fun &rest args)
  (let ((results '()) 
        (counter -1)
        (numberp))

    (setq results (cl-loop for (begin .  env) in 
                        (org-element-map (org-element-parse-buffer) 'latex-environment
                          (lambda (env)
                            (cons
                             (org-element-property :begin env)
                             (org-element-property :value env))))
                        collect
                        (cond
                         ((and (string-match "\\\\begin{equation}" env)
                               (not (string-match "\\\\tag{" env)))
                          (cl-incf counter)
                          (cons begin counter))
                         ((string-match "\\\\begin{align}" env)
                          (prog2
                              (cl-incf counter)
                              (cons begin counter)                          
                            (with-temp-buffer
                              (insert env)
                              (goto-char (point-min))
                              ;; \\ is used for a new line. Each one leads to a number
                              (cl-incf counter (count-matches "\\\\$"))
                              ;; unless there are nonumbers.
                              (goto-char (point-min))
                              (cl-decf counter (count-matches "\\nonumber")))))
                         (t
                          (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))
  
  (apply orig-fun args))

(advice-add 'org-create-formula-image :around #'org-renumber-environment)

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

(define-key org-mode-map (kbd "<up>") (lambda () (interactive) (custom/previous-line (org-in-src-block-p))))

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
