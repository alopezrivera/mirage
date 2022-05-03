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

(defun custom/org-at-ellipsis (&optional position)
  (and (custom/org-relative-line-heading-folded) (custom/at-point 'end-of-visual-line)))

(defun custom/org-relative-line-list (&optional number)
  (interactive)
  (custom/relative-line (lambda () (progn (beginning-of-line-text) (org-at-item-p)))  number))

(defun custom/org-relative-line-list-empty (&optional number)
  (custom/relative-line-regex "[[:blank:]]*[-+*]?[0-9.)]*[[:blank:]]+$" number))

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

(defun custom/org-subtree-empty ()
  "Return t if the current subtree consists of
a `custom/region-empty'."
  (if (org-element--cache-active-p)
      (let* ((heading (org-element-lineage
                       (or element (org-element-at-point))
                       '(headline) t))
	     (head (org-element-property :begin heading))
	     (next (org-element-property :end   heading)))
        (if (and heading next)
	    (progn (save-excursion (goto-char next)
				   (beginning-of-line 0)
				   (setq end (point)))
		   (if (= head end)
		       t
		     (save-excursion (goto-char head)
				     (beginning-of-line 2)
				     (custom/region-empty (point) next))))))))

(defun custom/org-end ()
  "Conditional end in Org Mode.

Default: `custom/end'

If `org-at-table-p', go to `org-table-end-of-field'."
  (cond ((org-at-table-p) (org-table-end-of-field 1))
	    (t                (end-of-visual-line))))

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

Else, if a multi-visual-line region is active and the cursor lies on a source code
block, home to `beginning-of-line'.

Else, if a region is active the cursor lies `custom/org-at-ellipsis', home to
`beginning-of-visual-line'.

Else, if the cursor lies `custom/org-at-ellipsis' (no active region), home to
the `beginning-of-line-text' of the heading's visual line.

Else, if the cursor lies on at heading or list, home to `beginning-of-line-text'.

Else, if the cursor lies in a source code block, and the current line is a wrapped
visual line, home to `beginning-of-visual-line'.

Else, if the cursor lies in a source code block, home `back-to-indentation'.

Else, if `org-at-table-p', home to `org-table-beginning-of-field'."
   (interactive)
   (cond ((and (custom/region-multiline-visual) (custom/org-relative-line-heading-or-list))  (beginning-of-visual-line))
         ((and (custom/region-multiline-visual) (org-in-src-block-p))                        (beginning-of-line))
         ((and (region-active-p) (custom/org-at-ellipsis))                                   (beginning-of-visual-line))
         ((custom/org-at-ellipsis)                        (progn (beginning-of-visual-line)  (beginning-of-line-text)))
         ((custom/org-relative-line-heading-or-list)                                         (beginning-of-line-text))
	     ((and (org-in-src-block-p) (> (custom/get-point 'beginning-of-visual-line)
					   (custom/get-point 'back-to-indentation)))             (beginning-of-visual-line))
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

;; Do not insert newline before Org Mode headings
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

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

(defun custom/org-delete-region ()
  "If the region starts at the beginning of an 
indented line and the cursor lies on an Org Mode
src block, delete the region and its indent plus 
one character."
  (interactive)
  (custom/@delete-region (org-in-src-block-p)))

(defun custom/org-nimble-delete-forward ()
  "Org Mode complement to `custom/nimble-delete-forward'."
  (interactive)
  (cond ((and (custom/org-at-ellipsis) (custom/org-relative-line-heading 1))  (progn (beginning-of-visual-line 2) (beginning-of-line-text) (delete-forward-char 1)))
	      (t (custom/nimble-delete-forward))))

(define-key org-mode-map (kbd "<deletechar>") 'custom/org-nimble-delete-forward)

(defun custom/org-nimble-delete-backward ()
  "Org Mode complement to `custom/nimble-delete-backward'."
  (interactive)
  (cond ((region-active-p)                                                                     (custom/org-delete-region))
	((and (custom/org-relative-line-heading-folded) (custom/at-point 'end-of-visual-line)) (progn (beginning-of-visual-line) (end-of-line) (delete-backward-char 1)))
	((or (custom/org-relative-line-heading-empty) (custom/org-relative-line-list-empty))   (delete-region (point) (custom/get-point 'end-of-line 0)))
        (t                                                                                     (custom/nimble-delete-backward))))

(define-key org-mode-map (kbd "<backspace>") 'custom/org-nimble-delete-backward)

;; (defun custom/org-smart-comment ()
;;   "`smart-comment' in modes derived from `prog-mode'."
;;   (interactive)
;;   (custom/@smart-comment (org-in-src-block-p)))

;; (define-key org-mode-map (kbd "M-;") #'custom/smart-comment)

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
    (if (org-in-src-block-p)
	(org-indent-line)
      (apply orig-fun args))))

(advice-add 'org-cycle :around #'custom/org-cycle)

;; org-return
(defun custom/org-return ()
  "Conditional `org-return'."
  (interactive)
  (cond ((custom/org-relative-line-list-empty)
	        (progn (delete-region
			(custom/get-point 'beginning-of-line)
			(custom/get-point 'end-of-line))
		       (org-return)))
	      ((and (or (custom/org-relative-line-list -1)
			(custom/relative-line-indented -1))
		    (bolp))
	       (org-return))
	      ((and (custom/org-relative-line-heading)
		    (custom/at-point (lambda ()
				       (beginning-of-visual-line)
				       (beginning-of-line-text))))
	       (save-excursion (beginning-of-visual-line)
			       (org-return t)))
	      ((and (custom/org-relative-line-heading)
		    (not (custom/org-at-ellipsis))
		    (not (custom/org-relative-line-heading-empty))
		    (eolp))
	       (progn (newline)
		      (org-return t)))
	      (t
	       (org-return t))))

(define-key org-mode-map (kbd "<return>") 'custom/org-return)

;; org-meta-return
(defun custom/org-meta-return ()
  "Conditional `org-meta-return'."
  (interactive)
  (cond ((custom/org-relative-line-list-empty)          (progn (org-meta-return) (next-line) (end-of-line)))
	      ((custom/org-relative-line-heading)             (progn (beginning-of-visual-line) (org-insert-heading-respect-content)))
	      ((custom/org-relative-line-list)                (progn (end-of-line) (org-meta-return)))
	      ((org-in-src-block-p)                           (progn (org-insert-heading-respect-content) (beginning-of-visual-line) (org-return) (beginning-of-line-text)))
	      (t                                              (org-meta-return))))

(define-key org-mode-map (kbd "C-<return>") #'custom/org-meta-return)

(defun custom/org-meta-arrows-h (orig-fun &rest args)
  "Paragraph indentation with `org-meta<arrows>'.
Furthermore, if a region is active and its
beginning lies on an Org Mode heading, create
a new region spanning from the `beginning-of-line'
where beg was found to the end of the original
region, and proceed to execute `org-meta<arrows>'."
  (interactive)
  (if (and (not (custom/org-relative-line-list))
           (custom/relative-line-list -1))
      (progn ;; If the paragraph is indented,
	         ;; assume it will have a visual
	         ;; indent as the one created by this
	         ;; function, and revert it before
	         ;; turning paragraph into item.
	         (if (custom/relative-line-indented)
		     (progn (right-char)
			    (setq back (point))
			    (beginning-of-line-text)
			    (insert " ")
			    (goto-char back)))
	         ;; Hitch the item ride
	         (org-toggle-item (point))
		 ;; If cursor has remained at bol,
		 ;; move to `beginning-of-line-text'
		 (if (bolp) (beginning-of-line-text))
		 ;; Execute `org-meta<arrow>'
		 (ignore-errors (apply orig-fun args))
		 ;; Drop off
		 (org-toggle-item (point))
		 ;; Ensure cursor remains at
		 ;; `beginning-of-line-text'
		 (if (bolp) (beginning-of-line-text))
		 (if (custom/relative-line-indented)
		     (progn (left-char 1)
			    (setq back (point))
			    (beginning-of-line-text)
			    (delete-backward-char 1)
			    (goto-char back)
			    ))
		 )
    ;; Furthermore, if a region is active and its
    ;; beginning lies on an Org Mode heading, create
    ;; a new region spanning from the `beginning-of-line'
    ;; where beg was found to the end of the original
    ;; region, and proceed to execute `org-meta<arrows>'.
    (if (region-active-p)
	      (let ((beg (region-beginning))
		    (end (region-end)))
		   (save-excursion (deactivate-mark)
				   (goto-char beg)
				   (if (custom/org-relative-line-heading)
				       (set-mark (custom/get-point 'beginning-of-line))
				     (set-mark beg))
				   (goto-char end)
				   (apply orig-fun args)
				   (deactivate-mark)
				   ))
      (apply orig-fun args))))

(advice-add 'org-metaleft :around #'custom/org-meta-arrows-h)
(advice-add 'org-metaright :around #'custom/org-meta-arrows-h)

(defun custom/org-edit-at-ellipsis (orig-fun &rest args)
  "Execute commands invoked at an Org Mode heading's
ellipsis in the first line under the heading."
  (if (custom/org-at-ellipsis)
      (progn (beginning-of-visual-line)
	           (org-show-children)
		   (end-of-line)
		   (org-return)
		   (apply orig-fun args))
    (apply orig-fun args)))

(dolist (fn '(org-yank
	            org-self-insert-command))
  (advice-add fn :around #'custom/org-edit-at-ellipsis))

(defun custom/org-insert-subheading ()
  "Support `org-insert-subheading' from any point in tree."
  (interactive)
  (if (org-current-level)
      (progn (if (not (= 1 (org-current-level)))
                 (progn (outline-up-heading 0)
                        (end-of-line)
                        (org-show-children)))
             (org-insert-subheading 0))
    (org-insert-subheading 0)))

(define-key org-mode-map (kbd "S-<return>") 'custom/org-insert-subheading)

(defun custom/org-insert-heading-respect-content (orig-fun &rest args)
  "Support `org-insert-heading-respect-content' from any point in tree."
  (if (org-current-level)
      (progn (if (not (= 1 (org-current-level)))
	               (outline-up-heading 0))
             (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'org-insert-heading-respect-content :around #'custom/org-insert-heading-respect-content)

(defun custom/org-insert-subheading-respect-content ()
  "`org-insert-subheading' respecting content."
  (interactive)
  (if (custom/org-relative-line-heading)
      (progn (beginning-of-visual-line)
	           (org-show-children)))
  (if (not (= 1 (org-current-level)))
      (outline-up-heading 0))
  (org-insert-subheading '(4)))

(define-key org-mode-map (kbd "M-<return>") 'custom/org-insert-subheading-respect-content)

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

(defun custom/org-indent-region ()
  (interactive)
  (save-excursion (org-babel-mark-block)
		      (org-indent-region (region-beginning) (region-end))
		      (deactivate-mark)))

(define-key org-mode-map (kbd "C-\\") 'custom/org-indent-region)

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
(tempo-define-template "latex"
		             '("#+NAME: eq:1" p "\n\\begin{equation}\n\\end{equation}" >)
			     "<eq"
			     "LaTeX equation template")

;; Code block structure templates
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Justify equation labels - [fleqn]
;; Preview page width      - 10.5cm
(setq org-format-latex-header
      "\\documentclass[fleqn]{article}\n\\usepackage[usenames]{color}\n[PACKAGES]\n[DEFAULT-PACKAGES]\n\\pagestyle{empty}             % do not remove\n% The settings below are copied from fullpage.sty\n\\setlength{\\textwidth}{10.5cm}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}")

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

;; Language packages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python     . t)))

;; Trigger org-babel-tangle when saving any org files in the config directory
(setq source-regex (list ".org" (replace-regexp-in-string "~" "/root" config-directory)))

(defun custom/org-babel-tangle-config()
  "Call org-babel-tangle when the Org  file in the current buffer is located in the config directory"
     (if (custom/match-regexs (expand-file-name buffer-file-name) source-regex)
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

(org-roam-db-autosync-mode)

;; Org Roam UI
(straight-use-package 'org-roam-ui)

(setq org-roam-ui-follow t)

;; Sync theme and UI
(setq org-roam-ui-sync-theme nil)

(setq org-roam-ui-open-on-start nil)

(setq org-roam-ui-update-on-save t)

;; Org Roam timestamps
(straight-use-package 'org-roam-timestamps)

;; Org Agenda log mode
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Org Agenda week view key binding
(global-set-key (kbd "C-c a") (lambda () (interactive) (org-agenda)))

;; Restart Org Agenda
(defun custom/org-agenda-restart ()
  (interactive)
  (org-agenda-quit) 
  (org-agenda))

;; Mark items as done
(defun custom/org-agenda-todo-done ()
  (interactive)
  (org-agenda-todo 'done))

;; Set custom Org Agenda key bindings
(defun custom/org-agenda-custom-bindings ()
  ;; (local-set-key (kbd "<escape>") 'org-agenda-quit)
  (local-set-key (kbd "C-a") #'custom/org-agenda-restart)
  (local-set-key (kbd "d")   #'custom/org-agenda-todo-done))

(add-hook 'org-agenda-mode-hook 'custom/org-agenda-custom-bindings)

;; Set Org Agenda files
(setq org-agenda-files '("/home/tasks.org"))

(setq org-tag-alist
      '((:startgroup)
	;; Put mutually exclusive tags here
	(:endgroup)
	("@errand"  . ?E)
	("@home"    . ?H)
	("@work"    . ?W)
	("agenda" . ?a)
	("planning" . ?p)
	("publish"  . ?P)
	("batch"    . ?b)
	("note"     . ?n)
	("idea"     . ?i)))

;; Define TODO keyword sequences
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(r)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;; Configure custom agenda views
(setq org-agenda-custom-commands
      
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
	
	("n" "Next Tasks"
	 ((todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))))

 	("W" "Work Tasks" tags-todo "+work-email")

	("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	 ((org-agenda-overriding-header "Low Effort Tasks")
	  (org-agenda-max-todos 20)
	  (org-agenda-files org-agenda-files)))

	("w" "Workflow Status"
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