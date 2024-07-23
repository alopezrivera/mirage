(defun mirage/org-hide-previous-subtree ()
  "Cycle previous Org Mode heading."
  (save-excursion (mirage/org-goto-heading-previous)
		  (outline-hide-subtree)))

(defun mirage/org-show (orig-fun &rest args)
  (if (mirage/org-at-ellipsis)
      (progn (mirage/org-goto-heading-bol) (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'org-show-subtree :around #'mirage/org-show)

(advice-add 'org-show-children :around #'mirage/org-show)

(defun mirage/org-show-minimum ()
  (if (or (mirage/org-relative-line-list-folded)
	        (mirage/org-relative-line-heading-folded))
      (progn (if (mirage/org-at-ellipsis)
		       (progn (beginning-of-visual-line) (end-of-line)))
	           (org-show-entry)
	           (if (mirage/org-heading-has-children) (org-show-children)))))

(defun mirage/org-cycle (orig-fun &rest args)
  "Conditional `org-cycle'.

Default: `org-cycle'

If cursor lies at `end-of-visual-line' of folded heading or list,
move cursor to `end-of-line' of the current visual line and then
call `org-cycle'.

If cursor lies at a paragraph directly under a list item and not
indented at the level of the previous list item, indent the paragraph."
  (interactive)
  (if (or (mirage/org-relative-line-list-folded) (mirage/org-relative-line-heading-folded))
      (if (= (point) (mirage/get-point 'end-of-visual-line))
	  (progn (beginning-of-visual-line)
		 (end-of-line)
		 (apply orig-fun args))
	(apply orig-fun args))
    (if (and (org-in-src-block-p) (not (mirage/org-at-keyword)))
	      (org-indent-line)
      (apply orig-fun args))))

(advice-add 'org-cycle :around #'mirage/org-cycle)

(defun mirage/c-cycle ()
  (interactive)
  (if (and (org-in-src-block-p) (not (invisible-p (point-at-eol))))
      (progn (org-babel-goto-src-block-head)
             (org-fold-hide-block-toggle))
    (org-fold-hide-subtree)))

(define-key org-mode-map (kbd "C-<tab>") #'mirage/c-cycle)

(defun mirage/org-identify-hidden-overlays (overlay &optional use-markers)
  (when (eq (overlay-get overlay 'invisible) 'outline)
    (let ((beg (overlay-start overlay))
          (end (overlay-end overlay)))
      (and beg end (> end beg)
           (if use-markers
               (cons (copy-marker beg)
                     (copy-marker end t))
             (cons beg end))))))

(defun mirage/org-get-outline-state (&optional use-markers)
  "Return a list of the locations of all outline overlays.
These are overlays with the `invisible' property value `outline'.
The return value is a list of cons cells, with start and stop
positions for each overlay.
If USE-MARKERS is set, return the positions as markers."
  (let (beg end)
    (org-with-wide-buffer
     (delq nil
       (mapcar 'mirage/org-identify-hidden-overlays
           (overlays-in (point-min) (point-max)))))))

(defun mirage/org-save-outline-state ()
  "Save org outline state in `mirage/org-outline-state'.
It can be recovered afterwards with `mirage/org-recover-outline-state'."
  (setq mirage/org-outline-state (mirage/org-get-outline-state t)))

(defvar-local mirage/org-outline-state nil
  "Variable to save the org outline.")
(put 'mirage/org-outline-state 'permanent-local t)

(defun mirage/org-set-outline-state (data)
  "Create visibility overlays for all positions in DATA.
DATA should have been made by `mirage/org-get-outline-state'."
  (org-with-wide-buffer
   (org-show-all)
   (dolist (c data) (org-flag-region (car c) (cdr c) t 'outline))))

(defun mirage/org-restore-outline-state ()
  "Restore Org Mode outline stored in `mirage/org-outline-state'."
  (when mirage/org-outline-state
    (mirage/org-set-outline-state mirage/org-outline-state)
    (setq mirage/org-outline-state nil)))

(defun mirage/org-mode (orig-fun &rest args)
  (if (string-equal major-mode "org-mode")
      (progn (mirage/org-save-outline-state)
             (apply orig-fun args)
             (mirage/org-restore-outline-state))
    (apply orig-fun args)))

(advice-add 'org-mode :around #'mirage/org-mode)

(provide 'mirage-extension-org-outline)
;;; mirage-org-outline.el ends here
