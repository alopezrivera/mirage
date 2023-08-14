(defun seaman/org-hide-previous-subtree ()
  "Cycle previous Org Mode heading."
  (save-excursion (seaman/org-goto-heading-previous)
		  (outline-hide-subtree)))

(defun seaman/org-show (orig-fun &rest args)
  (if (seaman/org-at-ellipsis)
      (progn (seaman/org-goto-heading-bol) (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'org-show-subtree :around #'seaman/org-show)

(advice-add 'org-show-children :around #'seaman/org-show)

(defun seaman/org-show-minimum ()
  (if (or (seaman/org-relative-line-list-folded)
	        (seaman/org-relative-line-heading-folded))
      (progn (if (seaman/org-at-ellipsis)
		       (progn (beginning-of-visual-line) (end-of-line)))
	           (org-show-entry)
	           (if (seaman/org-heading-has-children) (org-show-children)))))

(defun seaman/org-cycle (orig-fun &rest args)
  "Conditional `org-cycle'.

Default: `org-cycle'

If cursor lies at `end-of-visual-line' of folded heading or list,
move cursor to `end-of-line' of the current visual line and then
call `org-cycle'.

If cursor lies at a paragraph directly under a list item and not
indented at the level of the previous list item, indent the paragraph."
  (interactive)
  (if (or (seaman/org-relative-line-list-folded) (seaman/org-relative-line-heading-folded))
      (if (= (point) (seaman/get-point 'end-of-visual-line))
	  (progn (beginning-of-visual-line)
		 (end-of-line)
		 (apply orig-fun args))
	(apply orig-fun args))
    (if (and (org-in-src-block-p) (not (seaman/org-at-keyword)))
	      (org-indent-line)
      (apply orig-fun args))))

(advice-add 'org-cycle :around #'seaman/org-cycle)

(defun seaman/c-cycle ()
  (interactive)
  (if (and (org-in-src-block-p) (not (invisible-p (point-at-eol))))
      (progn (org-babel-goto-src-block-head)
             (org-fold-hide-block-toggle))
    (org-fold-hide-subtree)))

(define-key org-mode-map (kbd "C-<tab>") #'seaman/c-cycle)

(defun seaman/org-identify-hidden-overlays (overlay &optional use-markers)
  (when (eq (overlay-get overlay 'invisible) 'outline)
    (let ((beg (overlay-start overlay))
          (end (overlay-end overlay)))
      (and beg end (> end beg)
           (if use-markers
               (cons (copy-marker beg)
                     (copy-marker end t))
             (cons beg end))))))

(defun seaman/org-get-outline-state (&optional use-markers)
  "Return a list of the locations of all outline overlays.
These are overlays with the `invisible' property value `outline'.
The return value is a list of cons cells, with start and stop
positions for each overlay.
If USE-MARKERS is set, return the positions as markers."
  (let (beg end)
    (org-with-wide-buffer
     (delq nil
       (mapcar 'seaman/org-identify-hidden-overlays
           (overlays-in (point-min) (point-max)))))))

(defun seaman/org-save-outline-state ()
  "Save org outline state in `seaman/org-outline-state'.
It can be recovered afterwards with `seaman/org-recover-outline-state'."
  (setq seaman/org-outline-state (seaman/org-get-outline-state t)))

(defvar-local seaman/org-outline-state nil
  "Variable to save the org outline.")
(put seaman/org-outline-state 'permanent-local t)

(defun seaman/org-set-outline-state (data)
  "Create visibility overlays for all positions in DATA.
DATA should have been made by `seaman/org-get-outline-state'."
  (org-with-wide-buffer
   (org-show-all)
   (dolist (c data) (org-flag-region (car c) (cdr c) t 'outline))))

(defun seaman/org-restore-outline-state ()
  "Restore Org Mode outline stored in `seaman/org-outline-state'."
  (when seaman/org-outline-state
    (seaman/org-set-outline-state seaman/org-outline-state)
    (setq seaman/org-outline-state nil)))

(defun seaman/org-mode (orig-fun &rest args)
  (if (string-equal major-mode "org-mode")
      (progn (seaman/org-save-outline-state)
             (apply orig-fun args)
             (seaman/org-restore-outline-state))
    (apply orig-fun args)))

(advice-add 'org-mode :around #'seaman/org-mode)

(provide 'shapes-extension-org-outline)
;;; shapes-org-outline.el ends here
