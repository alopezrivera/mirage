;; Double end to go to the beginning of line
(defvar mirage/double-end-timeout 0.4)

(defun mirage/double-end ()
  "Move to end of visual line. If the command is repeated 
within `mirage/double-end-timeout' seconds, move to end
of line."
  (interactive)
  (let ((last-called (get this-command 'mirage/last-call-time)))
    (if (and (eq last-command this-command)
             (<= (time-to-seconds (time-since last-called)) mirage/double-end-timeout))
        (progn (beginning-of-visual-line) (end-of-line))
      (end-of-visual-line)))
  (put this-command 'mirage/last-call-time (current-time)))

(global-set-key (kbd "<end>") #'mirage/double-end)

(defun mirage/home ()
  "Conditional homing. 

Default: `beginning-of-line-text'

If the current line is empty, home to `beginning-of-line'.

If the current line holds a list item, home back to `beginning-of-line-text'.

If the current line is indented, home `back-to-indentation'.

If the current mode is derived from `prog-mode', home `back-to-indentation'.

If the current line is a wrapped visual line, home to
`beginning-of-visual-line'."
  (interactive)
  (cond ((mirage/relative-line-empty)    (beginning-of-line))
	((mirage/relative-line-list)     (beginning-of-line-text))
	((mirage/relative-line-indented) (back-to-indentation))
	((mirage/relative-line-wrapped)  (beginning-of-visual-line))
	((derived-mode-p 'prog-mode)     (back-to-indentation))
	((mirage/relative-line-wrapped)  (beginning-of-visual-line))
        (t                               (beginning-of-line-text))))

(defvar mirage/double-home-timeout 0.4)

(defun mirage/double-home ()
  "Dynamic homing command with a timeout of `mirage/double-home-timeout' seconds.
- Single press: `mirage/home' 
- Double press: `beginning-of-visual-line'"
  (interactive)
  (let ((last-called (get this-command 'mirage/last-call-time)))
    (if (and (eq last-command this-command)
	     (<= (time-to-seconds (time-since last-called)) mirage/double-home-timeout))
	(progn (beginning-of-visual-line)
	       (beginning-of-line-text))
      (mirage/home)))
  (put this-command 'mirage/last-call-time (current-time)))

(global-set-key (kbd "<home>") #'mirage/double-home)

(defun mirage/previous-line (orig-fun &rest args)
  "If a region is active and either the current mode is derived from
`prog-mode' or the cursor lies in an `org-babel' source code block,
arrow-up to `end-of-visual-line' of `previous-line'."
  (apply orig-fun args)
  (if (and (region-active-p)
           (or (derived-mode-p 'prog-mode)
               (and (string-equal major-mode "org-mode") (org-in-src-block-p))))
      (progn (point-to-register 'region-up-register)
             (end-of-visual-line))))

(advice-add 'previous-line :around #'mirage/previous-line)

(defun mirage/region-up-register ()
  "Move cursor to `region-up-register', defined in
`mirage/previous-line'."
  (interactive)
  (let ((end (region-end)))
    (ignore-errors (jump-to-register 'region-up-register))
    (set-register 'region-up-register nil)
    (push-mark end)))

(global-set-key (kbd "S-<home>") #'mirage/region-up-register)

(defun mirage/beginning-of-line-text (orig-fun &rest args)
  "Correctly go to `beginning-of-line-text' in numbered lists."
  (interactive)
  (let ((ordered-line-regex "^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]\\{1\\}"))
    (if (save-excursion (beginning-of-line)
			    (looking-at-p ordered-line-regex))
	    (progn (beginning-of-line)
		   (re-search-forward ordered-line-regex))
      (apply orig-fun args))))

(advice-add 'beginning-of-line-text :around #'mirage/beginning-of-line-text)

;; split and follow
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (other-window 1))
(global-set-key (kbd "C-x 2") #'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (other-window 1))
(global-set-key (kbd "C-x 3") #'split-and-follow-vertically)

(defvar mirage/window-previous nil
  "Selected window before the last window change.")

(defvar mirage/window-pre-command nil
  "Auxiliary variable containing the `selected-window'
before the execution of any command.")

(defun mirage/record-window-pre-command ()
  (setq mirage/window-pre-command (selected-window)))
(add-hook 'pre-command-hook #'mirage/record-window-pre-command)

(defun mirage/record-window-previous ()
  (let ((window-post (selected-window)))
    (if (not (eq window-post mirage/window-pre-command))
	      (setq mirage/window-previous mirage/window-pre-command))))
(add-hook 'post-command-hook #'mirage/record-window-previous)

(defun mirage/goto-window-previous ()
  (interactive)
  (let ((target  mirage/window-previous)
	      (current (selected-window)))
    (if target
	      (progn (select-window target)
		     (setq mirage/window-previous current)))))

(global-set-key (kbd "C-p") #'mirage/goto-window-previous)

(provide 'mirage-extension-navigation)
;;; mirage-navigation.el ends here
