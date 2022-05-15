;; Org Mode
(straight-use-package 'org)
(require 'org)

;; Delight
(delight 'org-indent-mode)

;; Startup with inline images
(setq org-startup-with-inline-images t)

;; `org-in-src-block-p' gives false positives as of Org Mode 9.5.3. For
;; this reason, determine if cursor in src block with the more reliable
;; `org-babel-where-is-src-block-head'
(advice-add 'org-in-src-block-p :override 'org-babel-where-is-src-block-head)

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

(defun custom/org-at-list-paragraph ()
  (and (not (custom/org-relative-line-heading))
       (not (custom/org-relative-line-list))
       (not (org-in-src-block-p))
       (custom/org-relative-line-list -1)))

(defun custom/org-after-list-or-indent ()
  (or (custom/org-relative-line-list -1) (custom/relative-line-indented -1)))

(defun custom/org-relative-line-list (&optional number)
  (interactive)
  (custom/relative-line (lambda () (progn (beginning-of-line-text) (org-at-item-p)))  number))

(defun custom/org-relative-line-list-empty (&optional number)
  (and (custom/org-relative-line-list)
       (custom/relative-line-regex "[[:blank:]]*[-+*]?[0-9.)]*[[:blank:]]+$" number)))

(defun custom/org-relative-line-list-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded list item."
  (interactive)
  (custom/relative-line (lambda () (and (org-at-item-p) (invisible-p (point-at-eol)))) number))

(defun custom/org-relative-line-heading (&optional number)
  (interactive)
  (custom/relative-line 'org-at-heading-p number))

(defun custom/org-relative-line-heading-empty (&optional number)
  (custom/relative-line (lambda () (beginning-of-line-text) (org-point-at-end-of-empty-headline)) number))

(defun custom/org-relative-line-heading-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded heading."
  (interactive)
  (custom/relative-line (lambda () (and (org-at-heading-p) (invisible-p (point-at-eol)))) number))

(defun custom/org-relative-line-heading-or-list (&optional number)
  (custom/relative-line 'org-at-heading-or-item-p number))

(defun custom/org-subtree-blank ()
  "Return t if the current subtree consists of
a `custom/region-empty'."
  (interactive)
  (apply #'custom/region-empty (custom/org-subtree-region)))

(defun custom/org-subtree-empty ()
  (interactive)
  (string-equal "" (custom/org-subtree-content)))

(defun custom/org-headings-follow ()
  (let ((pos (point)))
    (save-excursion (custom/org-goto-heading-next)
		           (and (not (= pos (point))) (custom/org-relative-line-heading)))))

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
       (custom/org-get-title-current-buffer)))

(defun custom/org-get-title-current-buffer ()
    (nth 1
     (assoc "TITLE"
      (org-element-map (org-element-parse-buffer 'greater-element)
          '(keyword)
        #'custom/get-keyword-key-value))))

(defun custom/org-set-outline-overlay-data (data)
  "Create visibility overlays for all positions in DATA.
DATA should have been made by `org-outline-overlay-data'."
  (org-with-wide-buffer
   (org-show-all)
   (dolist (c data) (org-flag-region (car c) (cdr c) t 'outline))))

(defvar-local custom/org-outline-state nil
  "Place for saving org outline state before reverting the buffer.")

(put 'custom/org-outline-state 'permanent-local t)

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
  (if (or (custom/org-at-ellipsis-h) (custom/org-at-ellipsis-l))
      (progn (beginning-of-visual-line) (end-of-line) (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'org-show-subtree :around #'custom/org-show)

(advice-add 'org-show-children :around #'custom/org-show)

(defun custom/org-show-minimum ()
  (if (or (custom/org-relative-line-list-folded)
	        (custom/org-relative-line-heading-folded))
      (progn (if (or (custom/org-at-ellipsis-h) (custom/org-at-ellipsis-l))
		       (progn (beginning-of-visual-line) (end-of-line)))
	           (if (custom/org-heading-has-children)
		       (org-show-children)
		     (org-show-subtree)))))

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

(defun custom/org-heading-margin ()
  "Return margin between current heading and next."
  (if (org-current-level)
      (let ((pos            (custom/get-point 'custom/org-goto-heading-bol))
	           (end-of-subtree (custom/get-point 'custom/org-goto-subtree-end))
		   (next-heading   (custom/get-point 'custom/org-goto-heading-next)))
	          (if (not (= pos end-of-subtree))
		      (buffer-substring-no-properties end-of-subtree next-heading)
		    ""))
    (if (custom/org-headings-follow)
	       (buffer-substring-no-properties (point) (custom/get-point 'custom/org-goto-heading-next))
      "")))

(defun custom/org-heading-margin-insert-prior ()
  "If the previous subtree is not empty,
insert a margin of 1 empty line."
  (let ((insert-margin
	 (save-excursion (org-backward-heading-same-level 1)
			 (not (custom/org-subtree-blank)))))
    (if insert-margin
      (progn (beginning-of-visual-line)
	            (org-return)
		    (beginning-of-line-text)))))

(defun custom/org-heading-margin-delete-post ()
  "Delete newline after new headings created by
`respect-content' heading commands."
  (apply 'delete-region (custom/org-subtree-region)))

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
  (cond ((not (org-current-level)) (insert "* "))
	      (t                         (funcall command)))
  (if margin (custom/org-heading-margin-insert-prior))
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
  (let ((margin (not (or (custom/org-relative-line-heading) (custom/org-relative-line-heading -1)))))
    (custom/org-insert-heading 'org-insert-heading margin)))

(defun custom/org-insert-subheading-at-point ()
  (interactive)
  (let ((margin (not (or (custom/org-relative-line-heading) (custom/org-relative-line-heading -1)))))
    (custom/org-insert-heading 'org-insert-subheading margin)))

(defun custom/org-insert-heading-after-subtree ()
  "Insert heading after current subtree. As
`org-insert-heading-respect-content' does not
behave well with folded Org Mode headings, if
the previous heading is folded:
1. Unfold the heading
2. Create the new heading after its subtree
3. Fold it back"
  (let ((margin (custom/regex-match-count "\n" (custom/org-heading-margin)))
	      (prev-same-level    (custom/get-point 'beginning-of-visual-line))
	      (prev-lower-level   (custom/get-point 'custom/org-goto-child-last))
	      (folded-same-level  (custom/org-relative-line-heading-folded))
	      (folded-lower-level (save-excursion (custom/org-goto-child-last) (custom/org-relative-line-heading-folded))))
    ;; Unfold if necessary
    (if folded-same-level  (org-show-subtree))
    (if folded-lower-level (save-excursion (custom/org-goto-subtree-end) (org-show-subtree)))
    
    ;; Insert heading
    (cond ((not (org-current-level)) (insert "* "))
	        (t                         (progn (custom/org-goto-heading-current) (org-insert-heading-respect-content))))
    (custom/org-heading-margin-delete-post)
    
    ;; Insert margin with previous heading
    (custom/org-heading-margin-insert-prior)
    
    ;; Fold back if necessary
    (if folded-same-level  (save-excursion (goto-char prev-same-level)  (outline-hide-subtree)))
    (if folded-lower-level (save-excursion (goto-char prev-lower-level) (outline-hide-subtree)))
    
    ;; Recover margin with following heading
    (if (> margin 1) (save-excursion (insert "\n")))))

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
	           (not (custom/region-empty)))                 (custom/org-delete-hungry))
	     ((or  (custom/org-at-ellipsis-h)
		   (custom/org-at-ellipsis-l))                  (progn (beginning-of-visual-line) (end-of-line) (delete-backward-char 1)))
	     ((and (or (custom/org-relative-line-heading-empty)
		       (custom/org-relative-line-list-empty))
		   (or (custom/relative-line-empty -1)
		       (custom/org-relative-line-heading -1)
		       (custom/org-relative-line-list -1)))     (delete-region (point) (custom/get-point 'end-of-line 0)))
	     ((or  (custom/org-relative-line-heading-empty)
		   (custom/org-relative-line-list-empty))       (delete-region (point) (custom/get-point 'beginning-of-visual-line)))
        (t                                                 (custom/nimble-delete-backward))))

(define-key org-mode-map (kbd "<backspace>") 'custom/org-nimble-delete-backward)

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

(require 'org-paragraph (concat config-directory "org-paragraph.el"))

(defun custom/org-meta-arrows-h (orig-fun &rest args)
  "Paragraph indentation with `org-meta<arrows>'.
Furthermore, if a region is active and its
beginning lies on an Org Mode heading,
`custom/org-command-expand-region' to execute ORIG-FUN."
  (interactive)
  (cond ((custom/org-at-list-paragraph) (custom/org-paragraph orig-fun args))
	    ((region-active-p)              (custom/org-indent-region orig-fun args))
	    (t                              (apply orig-fun args))))

(advice-add 'org-metaleft  :around #'custom/org-meta-arrows-h)
(advice-add 'org-metaright :around #'custom/org-meta-arrows-h)

(defun custom/org-meta-arrows-v (orig-fun &rest args)
  (interactive)
  (if (or (custom/org-at-ellipsis-h) (custom/org-at-ellipsis-l))
      (progn (beginning-of-visual-line) (end-of-line)))
  (apply orig-fun args)
  (if (custom/org-relative-line-heading-folded)
      (outline-hide-subtree)))

(advice-add 'org-metaup   :around #'custom/org-meta-arrows-v)
(advice-add 'org-metadown :around #'custom/org-meta-arrows-v)

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

(defun custom/org-indent--compute-prefixes ()
  "Recompute line prefixes for regular text to
match the indentation of the parent heading."
  (dotimes (n org-indent--deepest-level)
      (let ((indentation (if (= n 0) 0 1)))
        (aset org-indent--text-line-prefixes
	        n
	        (org-add-props
	           (concat (make-string (+ n indentation) ?\s))
		    nil 'face 'org-indent)))))

(advice-add 'org-indent--compute-prefixes :after #'custom/org-indent--compute-prefixes)

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

;; org-return
(defun custom/org-return ()
  "Conditional `org-return'."
  (interactive)
  (cond ((custom/org-relative-line-list-empty)          (progn (custom/delete-line) (org-return)))
	    ((custom/org-at-bol-list)                       (progn (beginning-of-visual-line) (org-return) (beginning-of-line-text)))
	    ((and (custom/org-after-list-or-indent) (bolp)) (org-return))
	    ((custom/org-at-bol-heading)                    (save-excursion (beginning-of-visual-line) (org-return t)))
	    ((custom/org-at-eol-heading)                    (progn (newline 2) (if (custom/org-subtree-blank) (progn (newline) (previous-line)))))
	    (t                                              (org-return t))))

(define-key org-mode-map (kbd "<return>") 'custom/org-return)

;; org-meta-return
(defun custom/org-control-return ()
  (interactive)
  (cond ((custom/org-relative-line-list-empty) (progn (org-meta-return) (next-line) (end-of-line)))
	    ((custom/org-relative-line-heading)    (custom/org-insert-heading-after-subtree))
	    ((custom/org-relative-line-list)       (progn (end-of-line) (org-meta-return)))
	    ((custom/org-at-list-paragraph)        (custom/org-paragraph-toggle))
	    (t                                     (custom/org-insert-heading-after-subtree))))

(define-key org-mode-map (kbd "C-<return>") #'custom/org-control-return)

(defun custom/org-meta-return ()
  (interactive)
  (custom/org-insert-subheading-after-subtree))

(define-key org-mode-map (kbd "M-<return>") 'custom/org-meta-return)

(defun custom/org-super-return ()
  (interactive)
  (custom/org-insert-subheading-at-point))

(define-key org-mode-map (kbd "S-<return>") 'custom/org-super-return)

(define-key org-mode-map (kbd "M-S-<return>") 'custom/org-insert-heading-at-point)

;; Required as of Org 9.2
(require 'org-tempo)

;; Spacing advice
(defun custom/tempo-breathe (orig-fun &rest args)
  "Add a margin of one newline above and below the content 
of org-tempo templates."
  (if (string-equal "marker" (type-of (apply orig-fun args)))
      (progn (newline)
	         (newline)
		 (previous-line))))

(advice-add 'tempo-complete-tag :around #'custom/tempo-breathe)

;; LaTeX structure templates
(tempo-define-template "latex-equation"
		          '("#+NAME: eq:" n "\\begin{equation}" p "\\end{equation}" >)
			  "<eq"
			  "LaTeX equation template")

(tempo-define-template "latex-derivation"
		          '("#+NAME: eq:" n "\\begin{equation}" n "\\arraycolsep=3pt\\def\\arraystretch{2.25}" n "\\begin{array}{lll}" p "\\end{array}" n "\\end{equation}" >)
			  "<de"
			  "LaTeX derivation template")

;; Code block structure templates
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

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
    ;; attempt going to last subheading of previous same-level heading
    (custom/org-goto-child-last)
    ;; if there was no previous same-level heading, go to parent if not at top
    (if (= (point) current) (custom/org-goto-heading-parent))))

;; Justify equation labels - [fleqn]
;; Preview page width      - 10.5cm
(setq org-format-latex-header
      (string-join '("\\documentclass[fleqn]{article}"
		        "\\usepackage[usenames]{color}"
			
			"\\usepackage{bm}"
			
			"\\pagestyle{empty}"
			"\\setlength{\\textwidth}{10.5cm}"
			"\\addtolength{\\textwidth}{-3cm}"
			"\\setlength{\\oddsidemargin}{1.5cm}"
			"\\addtolength{\\oddsidemargin}{-2.54cm}"
			"\\setlength{\\evensidemargin}{\\oddsidemargin}"
			"\\setlength{\\textheight}{\\paperheight}"
			"\\addtolength{\\textheight}{-\\headheight}"
			"\\addtolength{\\textheight}{-\\headsep}"
			"\\addtolength{\\textheight}{-\\footskip}"
			"\\addtolength{\\textheight}{-3cm}"
			"\\setlength{\\topmargin}{1.5cm}"
			"\\addtolength{\\topmargin}{-2.54cm}")
		   "\n"))

;; SVG LaTeX equation preview
(setq org-latex-create-formula-image-program 'dvisvgm)

;; Theme-specific LaTeX preview directory
(defun custom/latex-preview-directory ()
  (setq org-preview-latex-image-directory
   (concat config-directory "tmp/" "ltximg/" (custom/current-theme) "/")))

;; Reload LaTeX equation previews
(defun custom/latex-preview-reload ()
  "Reload all LaTeX previews in buffer,
ensuring the LaTeX preview directory
matches the current theme."
  (if (custom/in-mode "org-mode")
      (progn (org-latex-preview '(64))
	           (custom/latex-preview-directory)
		   (org-latex-preview '(16)))))

(add-hook 'org-mode-hook #'custom/latex-preview-reload)

;; Continuous numbering of Org Mode equations
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
                              (incf counter)
                              (cons begin counter)                          
                            (with-temp-buffer
                              (insert env)
                              (goto-char (point-min))
                              ;; \\ is used for a new line. Each one leads to a number
                              (incf counter (count-matches "\\\\$"))
                              ;; unless there are nonumbers.
                              (goto-char (point-min))
                              (decf counter (count-matches "\\nonumber")))))
                         (t
                          (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))
  
  (apply orig-fun args))

(advice-add 'org-create-formula-image :around #'org-renumber-environment)

;; org-fragtog
(use-package org-fragtog)

(add-hook 'org-mode-hook 'org-fragtog-mode)

(require 'org-diary (concat config-directory "org-diary.el"))

;; Language packages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python     . t)))

;; Trigger org-babel-tangle when saving any org files in the config directory
(setq source-regex (list ".org" (replace-regexp-in-string "~" "/root" config-directory)))

(defun custom/org-babel-tangle-config()
  "Call org-babel-tangle when the Org  file in the current buffer is located in the config directory"
     (if (custom/regex-match-patterns (expand-file-name buffer-file-name) source-regex)
     ;; Tangle ommitting confirmation
     (let ((org-confirm-babel-evaluate nil)) (org-babel-tangle)))
)
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'custom/org-babel-tangle-config)))

(defun custom/org-fix-bleed-end-line-block (from to flag spec)
  "Toggle fontification of last char of block end lines when cycling.

This avoids the bleeding of `org-block-end-line' when block is
folded."
  (when (and (eq spec 'org-hide-block)
             (/= (point-max) to))
    (save-excursion
      (if flag
          (font-lock-unfontify-region to (1+ to))
        (font-lock-flush to (1+ to))))))

(advice-add 'org-flag-region :after #'custom/org-fix-bleed-end-line-block)

(defun custom/org-fix-bleed-end-line-cycle (state)
  "Toggle fontification of last char of block lines when cycling.

This avoids the bleeding of `org-block-end-line' when outline is
folded."
  (save-excursion
    (when org-fontify-whole-block-delimiter-line
      (let ((case-fold-search t)
            beg end)
        (cond ((memq state '(overview contents all))
               (setq beg (point-min)
                     end (point-max)))
              ((memq state '(children folded subtree))
               (setq beg (point)
                     end (org-end-of-subtree t t))))
        (when beg           ; should always be true, but haven't tested enough
          (goto-char beg)
          (while (search-forward "#+end" end t)
            (end-of-line)
            (unless (= (point) (point-max))
              (if (org-invisible-p (1- (point)))
                  (font-lock-unfontify-region (point) (1+ (point)))
                (font-lock-flush (point) (1+ (point)))))))))))

(add-hook 'org-cycle-hook #'custom/org-fix-bleed-end-line-cycle)

(global-set-key (kbd "C-x C-x") 'org-babel-execute-src-block)

;; Set indentation of code blocks to 0
(setq org-edit-src-content-indentation 0)

;; Indent code blocks appropriately when inside headers
(setq org-src-preserve-indentation     nil)

;; Make code indentation reasonable
(setq org-src-tab-acts-natively        t)

;; Suppress security confirmation when evaluating code
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "python"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Org Roam
(straight-use-package 'org-roam)

;; Directory
(setq org-roam-directory "/home/roam")

;; Find node
(global-set-key (kbd "C-c n") 'org-roam-node-find)

;; Insert reference
(global-set-key (kbd "C-c i") 'org-roam-node-insert)

(org-roam-db-autosync-mode)

(add-hook 'org-roam-find-file-hook 'variable-pitch-mode)

;; Org Roam UI
(straight-use-package 'org-roam-ui)

(setq org-roam-ui-follow t)

;; Sync theme and UI
(setq org-roam-ui-sync-theme nil)

(setq org-roam-ui-open-on-start nil)

(setq org-roam-ui-update-on-save t)

;; Org Roam timestamps
(straight-use-package 'org-roam-timestamps)

;; Org Agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; Set Org Agenda files
(with-eval-after-load 'org-agenda
  (setq org-agenda-files '("~/.emacs.d/"
			       "/home/dfki/backlog.org")))

(defmacro custom/org-agenda-bind (key command)
  `(with-eval-after-load 'org-agenda
       (define-key org-agenda-mode-map (kbd ,key) ,command)))

;; Mark items as done
(defun custom/org-agenda-todo-done ()
  (interactive)
  (org-agenda-todo 'done))

(custom/org-agenda-bind "d" 'custom/org-agenda-todo-done)

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	      ((agenda "" ((org-deadline-warning-days 7)))
	       (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
	       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	     ("n" "Next Tasks"
	      ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))

	     ("w" "Work Tasks" tags-todo "work")

	     ("e" "Emacs Tasks" tags-todo "emacs")

	     ("z" "Low Effort" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	      ((org-agenda-overriding-header "Low Effort Tasks")
	       (org-agenda-max-todos 20)
	       (org-agenda-files org-agenda-files)))

	     ("s" "Workflow Status"
	      ((todo "WAIT"
		     ((org-agenda-overriding-header "Waiting on External")
		      (org-agenda-files org-agenda-files)))
	       (todo "REVIEW"
		     ((org-agenda-overriding-header "In Review")
		      (org-agenda-files org-agenda-files)))
	       (todo "PLAN"
		     ((org-agenda-overriding-header "In Planning")
		      (org-agenda-todo-list-sublevels nil)
		      (org-agenda-files org-agenda-files)))
	       (todo "BACKLOG"
		     ((org-agenda-overriding-header "Project Backlog")
		      (org-agenda-todo-list-sublevels nil)
		      (org-agenda-files org-agenda-files)))
	       (todo "READY"
		     ((org-agenda-overriding-header "Ready for Work")
		      (org-agenda-files org-agenda-files)))
	       (todo "ACTIVE"
		     ((org-agenda-overriding-header "Active Projects")
		      (org-agenda-files org-agenda-files)))
	       (todo "COMPLETED"
		     ((org-agenda-overriding-header "Completed Projects")
		      (org-agenda-files org-agenda-files)))
	       (todo "CANC"
		     ((org-agenda-overriding-header "Cancelled Projects")
		      (org-agenda-files org-agenda-files)))))))

;; Org Agenda log mode
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(custom/org-agenda-bind "<tab>" 'org-agenda-recenter)

(setq org-tag-alist
      '((:startgroup)
	;; Put mutually exclusive tags here
	(:endgroup)
	("errand"   . ?e)
	("home"     . ?h)
	("work"     . ?w)
	("agenda"   . ?a)
	("planning" . ?p)
	("publish"  . ?P)
	("batch"    . ?b)
	("note"     . ?n)
	("idea"     . ?i)))

;; Define TODO keyword sequences
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)")
	    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(r)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(straight-use-package 'calfw)
(straight-use-package 'calfw-org)
(straight-use-package 'calfw-ical)

;; org-agenda configuration is lost otherwise
(with-eval-after-load 'org-agenda
  (require 'calfw-org)
  (require 'calfw-ical))

(defun custom/org-calendar ()
  "Open `calfw' Org Agenda calendar."
  (interactive)
  (require 'org-agenda)
  (let ((inhibit-message t))
       (cfw:open-org-calendar)))

(global-set-key (kbd "C-c c") 'custom/org-calendar)
