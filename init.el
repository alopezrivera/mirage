;;; -*- lexical-binding: t; -*-

(setq initial-buffer-choice "~/.emacs.d/theme.org")

;; Initial frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width  . 70))

;; Default directory
(setq default-directory "~/.emacs.d/")

;; Config directory
(setq config-directory "~/.emacs.d/")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			       ("org"   . "https://orgmode.org/elpa/")
			       ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; use-package
(require 'use-package)

;; If true, Emacs will attempt to download packages in use-package declarations
(setq use-package-always-ensure t)

;; Customize interface code blocks
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defun <> (a b c)
  (and (> b a) (> c b)))

(defun custom/in-mode (mode)
  "Return t if Org Mode is currently active."
  (string-equal major-mode mode))

;; Retrieve active region
(defun custom/active-region (beg end)
  (set-mark beg)
  (goto-char end)
  (activate-mark)
  )

(defun custom/match-regexs (string patterns)
  "Return t if all provided regex PATTERNS
(provided as a list) match STRING."
  (cl-loop for pattern in patterns
	   if (not (string-match pattern string))
	     return nil
	   finally return t))

;; Retrieve current theme
(defun custom/current-theme ()
  (substring (format "%s" (nth 0 custom-enabled-themes))))

;; Inhibit startup message
(setq inhibit-startup-message t)

;; Disable visible scroll bar
(scroll-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable tooltips
(tooltip-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Enable visual bell
(setq visible-bell t)

(defun custom/hide-modeline ()
  (interactive)
  (if mode-line-format
      (setq mode-line-format nil)
    (doom-modeline-mode)))

(global-set-key (kbd "M-m") #'custom/hide-modeline)

;; Line numbers: display globally
(global-display-line-numbers-mode t)

;; Display column number
(column-number-mode)

;; Exceptions
(dolist (mode '(org-mode-hook
		    term-mode-hook
		    shell-mode-hook
		    eshell-mode-hook
		    undo-tree-visualizer-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set width of side fringes
(set-fringe-mode 0)

;; Load Swiper
(use-package swiper)

(require 'swiper)

;; Smart search
(defun custom/search-region (beg end)
  "Search selected region with swiper-isearch."
  (swiper-isearch (buffer-substring-no-properties beg end)))

(defun custom/smart-search (beg end)
  "Search for selected regions. If none are, call swiper-isearch."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (deactivate-mark)
  (if (and beg end)
      (custom/search-region beg end)
    (swiper-isearch)))

(define-key global-map (kbd "C-s") #'custom/smart-search)

(defun custom/narrow-and-search (beg end)
  (narrow-to-region beg end)
  (deactivate-mark)
  (swiper-isearch))

(defun custom/search-in-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (custom/narrow-and-search beg end)
    (swiper-isearch)))

(define-key global-map (kbd "C-x C-x") #'custom/search-in-region)

;; M-RET: multiple-cursors-mode
(define-key swiper-map (kbd "M-<return>") 'swiper-mc)

;; Ivy completion framework
(use-package counsel)
(use-package ivy
  :delight ivy-mode
  :bind (:map ivy-minibuffer-map
	      ("TAB" . ivy-alt-done)
	      ("C-l" . ivy-alt-done)
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)
	      :map ivy-switch-buffer-map
	      ("C-k" . ivy-previous-line)
	      ("C-l" . ivy-done)
	      ("C-d" . ivy-switch-buffer-kill)
	      :map ivy-reverse-i-search-map
	      ("C-k" . ivy-previous-line)
	      ("C-d" . ivy-reverse-i-search-kill))
  :init (ivy-mode 1))

;; Completion candidate descriptions
(use-package ivy-rich
  :bind
  (("<menu>" . counsel-M-x))
  :init (ivy-rich-mode 1))

;; Command suggestions
(use-package which-key
  :delight which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1.0))

;; Replace description key bindings by their helpful equivalents
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key))

(use-package command-log-mode
  :delight command-log-mode)
(global-command-log-mode)

;; Return to indentation
(global-set-key (kbd "S-<home>") 'back-to-indentation)

;; Counsel buffer switching
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Create new frame
(global-set-key (kbd "C-S-n") 'make-frame-command)

;; Make ESC quit present window and bury its buffer
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Unset secondary overlay key bindings
(global-unset-key [M-mouse-1])
(global-unset-key [M-drag-mouse-1])
(global-unset-key [M-down-mouse-1])
(global-unset-key [M-mouse-3])
(global-unset-key [M-mouse-2])

;; Unset mouse bindings
(global-unset-key [C-mouse-1])
(global-unset-key [C-down-mouse-1])

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-."         . mc/mark-next-like-this)
	 ("C-;"         . mc/mark-previous-like-this)
	 ("C-<mouse-1>" . mc/add-cursor-on-click))
  )

;; Load package
(require 'multiple-cursors)

;; Unknown commands file
(setq mc/list-file "~/.emacs.d/mc-lists.el")

;; Return as usual
(define-key mc/keymap (kbd "<return>")       'electric-newline-and-maybe-indent)

;; Exit multiple-cursors-mode
(define-key mc/keymap (kbd "<escape>")       'multiple-cursors-mode)
(define-key mc/keymap (kbd "<mouse-1>")      'multiple-cursors-mode)
(define-key mc/keymap (kbd "<down-mouse-1>")  nil)

;; Ensure rectangular-region-mode is loaded
(require 'rectangular-region-mode)

;; Save rectangle to kill ring
(define-key rectangular-region-mode-map (kbd "<mouse-3>") 'kill-ring-save)

;; Yank rectangle
(global-set-key (kbd "S-<mouse-3>") 'yank-rectangle)

;; Enter multiple-cursors-mode
(defun custom/rectangular-region-multiple-cursors ()
  (interactive)
  (rrm/switch-to-multiple-cursors)
  (deactivate-mark))

(define-key rectangular-region-mode-map (kbd "<return>") #'custom/rectangular-region-multiple-cursors)

;; Exit rectangular-region-mode
(define-key rectangular-region-mode-map (kbd "<escape>") 'rrm/keyboard-quit)
(define-key rectangular-region-mode-map (kbd "<mouse-1>") 'rrm/keyboard-quit)

;; Multiple cursor rectangle definition mouse event
(defun custom/smart-mouse-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (set-rectangular-region-anchor)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(global-set-key (kbd "M-<down-mouse-1>") #'custom/smart-mouse-rectangle)

(defun custom/smart-comment ()
  "Comments out the current line if no region is selected.
If the cursor stands on an opening parenthesis and Emacs Lisp 
mode is active, the region of the corresponding s expression 
is selected and commented out.
If a region is selected, it comments out the region, from 
the start of the top line of the region, to the end to its 
last line."
  (interactive)
  (let (beg end)
    (if (region-active-p)

	;; If the beginning and end of the region are in
	;; the same line, select entire line
	(if (= (count-lines (region-beginning) (region-end)) 1)
	    (setq beg (line-beginning-position) end (line-end-position))
	  ;; Else, select region from the start of its first
	  ;; line to the end of its last.
          (setq beg (save-excursion (goto-char (region-beginning)) (line-beginning-position))
		end (save-excursion (goto-char (region-end)) (line-end-position))))
      
      ;; Else, select line
      (setq beg (line-beginning-position) end (line-end-position)))


    ;; Comment or uncomment region
    ;; If Org Mode is active
    (if (custom/in-mode "org-mode")
	(if (org-in-src-block-p)
	    ;; Manage Org Babel misbehavior with comment-or-uncomment-region
	    (org-comment-dwim (custom/active-region beg end))
	  (comment-or-uncomment-region beg end))
      ;; Else, proceed regularly
      (comment-or-uncomment-region beg end))

    ;; Move to the beginning of the next line
    (move-beginning-of-line 2)))

(global-set-key (kbd "M-;") #'custom/smart-comment)

;; Create binding for evaluating buffer
(global-set-key (kbd "C-x e") 'eval-buffer)

;; Enable rainbow delimiters on all programming modes
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Unset secondary overlay key bindings
(global-unset-key [M-mouse-1])
(global-unset-key [M-drag-mouse-1])
(global-unset-key [M-down-mouse-1])
(global-unset-key [M-mouse-3])
(global-unset-key [M-mouse-2])

;; Unset mouse bindings
(global-unset-key [C-mouse-1])
(global-unset-key [C-down-mouse-1])

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-."         . mc/mark-next-like-this)
	 ("C-;"         . mc/mark-previous-like-this)
	 ("C-<mouse-1>" . mc/add-cursor-on-click))
  )

;; Load package
(require 'multiple-cursors)

;; Unknown commands file
(setq mc/list-file "~/.emacs.d/mc-lists.el")

;; Return as usual
(define-key mc/keymap (kbd "<return>")       'electric-newline-and-maybe-indent)

;; Exit multiple-cursors-mode
(define-key mc/keymap (kbd "<escape>")       'multiple-cursors-mode)
(define-key mc/keymap (kbd "<mouse-1>")      'multiple-cursors-mode)
(define-key mc/keymap (kbd "<down-mouse-1>")  nil)

;; Ensure rectangular-region-mode is loaded
(require 'rectangular-region-mode)

;; Save rectangle to kill ring
(define-key rectangular-region-mode-map (kbd "<mouse-3>") 'kill-ring-save)

;; Yank rectangle
(global-set-key (kbd "S-<mouse-3>") 'yank-rectangle)

;; Enter multiple-cursors-mode
(defun custom/rectangular-region-multiple-cursors ()
  (interactive)
  (rrm/switch-to-multiple-cursors)
  (deactivate-mark))

(define-key rectangular-region-mode-map (kbd "<return>") #'custom/rectangular-region-multiple-cursors)

;; Exit rectangular-region-mode
(define-key rectangular-region-mode-map (kbd "<escape>") 'rrm/keyboard-quit)
(define-key rectangular-region-mode-map (kbd "<mouse-1>") 'rrm/keyboard-quit)

;; Multiple cursor rectangle definition mouse event
(defun custom/smart-mouse-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (set-rectangular-region-anchor)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(global-set-key (kbd "M-<down-mouse-1>") #'custom/smart-mouse-rectangle)

(defun custom/smart-comment ()
  "Comments out the current line if no region is selected.
If the cursor stands on an opening parenthesis and Emacs Lisp 
mode is active, the region of the corresponding s expression 
is selected and commented out.
If a region is selected, it comments out the region, from 
the start of the top line of the region, to the end to its 
last line."
  (interactive)
  (let (beg end)
    (if (region-active-p)

	;; If the beginning and end of the region are in
	;; the same line, select entire line
	(if (= (count-lines (region-beginning) (region-end)) 1)
	    (setq beg (line-beginning-position) end (line-end-position))
	  ;; Else, select region from the start of its first
	  ;; line to the end of its last.
          (setq beg (save-excursion (goto-char (region-beginning)) (line-beginning-position))
		end (save-excursion (goto-char (region-end)) (line-end-position))))
      
      ;; Else, select line
      (setq beg (line-beginning-position) end (line-end-position)))


    ;; Comment or uncomment region
    ;; If Org Mode is active
    (if (custom/in-mode "org-mode")
	(if (org-in-src-block-p)
	    ;; Manage Org Babel misbehavior with comment-or-uncomment-region
	    (org-comment-dwim (custom/active-region beg end))
	  (comment-or-uncomment-region beg end))
      ;; Else, proceed regularly
      (comment-or-uncomment-region beg end))

    ;; Move to the beginning of the next line
    (move-beginning-of-line 2)))

(global-set-key (kbd "M-;") #'custom/smart-comment)

(global-set-key (kbd "C-`") 'widen)

;; Undo Tree
(use-package undo-tree
  :bind (("M-/" . undo-tree-visualize)
         :map undo-tree-visualizer-mode-map
         ("RET" . undo-tree-visualizer-quit)
         ("ESC" . undo-tree-visualizer-quit))
  :config
  (global-undo-tree-mode))

;; Visualize in side buffer
(defun custom/undo-tree-split-side-by-side (original-function &rest args)
  "Split undo-tree side-by-side"
  (let ((split-height-threshold nil)
        (split-width-threshold 0))
    (apply original-function args)))

(advice-add 'undo-tree-visualize :around #'custom/undo-tree-split-side-by-side)

;; ;; Undo tree command
;; (defun custom/undo-tree ()
;;   (interactive)
;;   (undo-tree-visualize))

;; (global-set-key (kbd "M-/") #'custom/undo-tree)

;; Increase kill ring size
(setq kill-ring-max 200)

;; Copy region with S-left click
(global-set-key (kbd "S-<mouse-1>")      'mouse-save-then-kill)
(global-set-key (kbd "S-<down-mouse-1>")  nil)

;; Paste with mouse right click
(global-set-key (kbd "<mouse-3>")        'yank)
(global-set-key (kbd "<down-mouse-3>")    nil)

(use-package magit)

;; List indentation
(setq-default org-list-indent-offset 1)

;; Render inline images when opening org files
(setq org-startup-with-inline-images t)

;; Load Org Mode
(use-package org
  :delight org-indent-mode
  )

;; Required as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
(add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py"  . "src python"))

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

;; LaTeX preview hook
(add-hook 'org-mode-hook 'org-latex-preview)

;; SVG LaTeX equation preview
(setq org-latex-create-formula-image-program 'dvisvgm)

;; Theme-specific LaTeX preview directory
(defun custom/latex-preview-directory ()
  (setq org-preview-latex-image-directory
   (concat config-directory "tmp/" "ltximg/" (custom/current-theme) "/")))

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
(setq org-agenda-files '("~/.emacs.d/tasks.org"
			 "~/.emacs.d/contacts.org"))

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

(require 'theme (concat config-directory "theme.el"))

(defvar after-enable-theme-hook nil
   "Normal hook run after enabling a theme.")

(defun run-after-enable-theme-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)

;; Reload Org Mode
(defun custom/org-theme-reload ()
  (if (custom/in-mode "org-mode")
      (org-mode)))

(add-hook 'after-enable-theme-hook #'custom/org-theme-reload)

;; LaTeX equation preview directory
(defun custom/switch-latex-preview-directory ()
  "Deactivate LaTeX previews in current buffer,
change LaTeX preview directory and reactivate
LaTeX previews in current buffer."
  (org-latex-preview '(4))
  (custom/latex-preview-directory)
  (org-latex-preview '(16)))

(add-hook 'after-enable-theme-hook #'custom/switch-latex-preview-directory)

;; Conclude initialization file
(provide 'init)
