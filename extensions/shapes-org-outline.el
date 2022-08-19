;;; -*- lexical-binding: t; -*-

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

(defun custom/org-get-outline-state (&optional use-markers)
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
  (setq custom/org-outline-state (custom/org-get-outline-state t)))

(defvar-local custom/org-outline-state nil
  "Place for saving org outline state before reverting the buffer.")

(put 'custom/org-outline-state 'permanent-local t)

(defun custom/org-set-outline-state (data)
  "Create visibility overlays for all positions in DATA.
DATA should have been made by `org-get-stateay-data'."
  (org-with-wide-buffer
   (org-show-all)
   (dolist (c data) (org-flag-region (car c) (cdr c) t 'outline))))

(defun custom/org-restore-outline-state ()
  "Save org outline state in `custom/org-outline-state'.
It can be recovered afterwards with `custom/org-recover-outline-state'."
  (when custom/org-outline-state
    (custom/org-set-outline-state custom/org-outline-state)
    (setq custom/org-outline-state nil)))

(defun custom/org-mode (orig-fun &rest args)
  (if (string-equal major-mode "org-mode")
      (progn (custom/org-save-outline-state)
	           (apply orig-fun args)
		   (custom/org-restore-outline-state))
    (apply orig-fun args)))

(advice-add 'org-mode :around #'custom/org-mode)

(provide 'shapes-extension-org-outline)
;;; shapes-org-outline.el ends here
