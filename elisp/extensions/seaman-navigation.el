;; Double end to go to the beginning of line
(defvar seaman/double-end-timeout 0.4)

(defun seaman/double-end ()
  "Move to end of visual line. If the command is repeated 
within `seaman/double-end-timeout' seconds, move to end
of line."
  (interactive)
  (let ((last-called (get this-command 'seaman/last-call-time)))
    (if (and (eq last-command this-command)
             (<= (time-to-seconds (time-since last-called)) seaman/double-end-timeout))
        (progn (beginning-of-visual-line) (end-of-line))
      (end-of-visual-line)))
  (put this-command 'seaman/last-call-time (current-time)))

(global-set-key (kbd "<end>") #'seaman/double-end)

(defun seaman/home ()
  "Conditional homing. 

Default: `beginning-of-line-text'

If the current line is empty, home to `beginning-of-line'.

If the current line holds a list item, home back to `beginning-of-line-text'.

If the current line is indented, home `back-to-indentation'.

If the current mode is derived from `prog-mode', home `back-to-indentation'.

If the current line is a wrapped visual line, home to
`beginning-of-visual-line'."
  (interactive)
  (cond ((seaman/relative-line-empty)    (beginning-of-line))
	((seaman/relative-line-list)     (beginning-of-line-text))
	((seaman/relative-line-indented) (back-to-indentation))
	((seaman/relative-line-wrapped)  (beginning-of-visual-line))
	((derived-mode-p 'prog-mode)     (back-to-indentation))
	((seaman/relative-line-wrapped)  (beginning-of-visual-line))
        (t                               (beginning-of-line-text))))

(defvar seaman/double-home-timeout 0.4)

(defun seaman/double-home ()
  "Dynamic homing command with a timeout of `seaman/double-home-timeout' seconds.
- Single press: `seaman/home' 
- Double press: `beginning-of-visual-line'"
  (interactive)
  (let ((last-called (get this-command 'seaman/last-call-time)))
    (if (and (eq last-command this-command)
	     (<= (time-to-seconds (time-since last-called)) seaman/double-home-timeout))
	(progn (beginning-of-visual-line)
	       (beginning-of-line-text))
      (seaman/home)))
  (put this-command 'seaman/last-call-time (current-time)))

(global-set-key (kbd "<home>") #'seaman/double-home)

(defun seaman/previous-line (orig-fun &rest args)
  "If a region is active and either the current mode is derived from
`prog-mode' or the cursor lies in an `org-babel' source code block,
arrow-up to `end-of-visual-line' of `previous-line'."
  (apply orig-fun args)
  (if (and (region-active-p)
           (or (derived-mode-p 'prog-mode)
               (and (string-equal major-mode "org-mode") (org-in-src-block-p))))
      (progn (point-to-register 'region-up-register)
             (end-of-visual-line))))

(advice-add 'previous-line :around #'seaman/previous-line)

(defun seaman/region-up-register ()
  "Move cursor to `region-up-register', defined in
`seaman/previous-line'."
  (interactive)
  (let ((end (region-end)))
    (ignore-errors (jump-to-register 'region-up-register))
    (set-register 'region-up-register nil)
    (push-mark end)))

(global-set-key (kbd "S-<home>") #'seaman/region-up-register)

(defun seaman/beginning-of-line-text (orig-fun &rest args)
  "Correctly go to `beginning-of-line-text' in numbered lists."
  (interactive)
  (let ((ordered-line-regex "^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]\\{1\\}"))
    (if (save-excursion (beginning-of-line)
			    (looking-at-p ordered-line-regex))
	    (progn (beginning-of-line)
		   (re-search-forward ordered-line-regex))
      (apply orig-fun args))))

(advice-add 'beginning-of-line-text :around #'seaman/beginning-of-line-text)

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

(defvar seaman/window-previous nil
  "Selected window before the last window change.")

(defvar seaman/window-pre-command nil
  "Auxiliary variable containing the `selected-window'
before the execution of any command.")

(defun seaman/record-window-pre-command ()
  (setq seaman/window-pre-command (selected-window)))
(add-hook 'pre-command-hook #'seaman/record-window-pre-command)

(defun seaman/record-window-previous ()
  (let ((window-post (selected-window)))
    (if (not (eq window-post seaman/window-pre-command))
	      (setq seaman/window-previous seaman/window-pre-command))))
(add-hook 'post-command-hook #'seaman/record-window-previous)

(defun seaman/goto-window-previous ()
  (interactive)
  (let ((target  seaman/window-previous)
	      (current (selected-window)))
    (if target
	      (progn (select-window target)
		     (setq seaman/window-previous current)))))

(global-set-key (kbd "C-p") #'seaman/goto-window-previous)

(provide 'seaman-extension-navigation)
;;; seaman-navigation.el ends here
