(straight-use-package 'org)
(require 'org)

;; org modules
(require 'org-inlinetask)

(require 'el-patch)

(el-patch-feature org)
(el-patch-defun org-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (el-patch-remove (org-fold-check-before-invisible-edit 'insert))
  (cond
   ((and org-use-speed-commands
	 (let ((kv (this-command-keys-vector)))
	   (setq org-speed-command
		 (run-hook-with-args-until-success
		  'org-speed-command-hook
		  (make-string 1 (aref kv (1- (length kv))))))))
    (cond
     ((commandp org-speed-command)
      (setq this-command org-speed-command)
      (call-interactively org-speed-command))
     ((functionp org-speed-command)
      (funcall org-speed-command))
     ((consp org-speed-command)
      (eval org-speed-command t))
     (t (let (org-use-speed-commands)
	  (call-interactively 'org-self-insert-command)))))
   ((and
     (= N 1)
     (not (org-region-active-p))
     (org-at-table-p)
     (progn
       ;; Check if we blank the field, and if that triggers align.
       (and (featurep 'org-table)
	    org-table-auto-blank-field
	    (memq last-command
		  '(org-cycle org-return org-shifttab org-ctrl-c-ctrl-c))
	    (if (or (eq (char-after) ?\s) (looking-at "[^|\n]*  |"))
		;; Got extra space, this field does not determine
		;; column width.
		(let (org-table-may-need-update) (org-table-blank-field))
	      ;; No extra space, this field may determine column
	      ;; width.
	      (org-table-blank-field)))
       t)
     (looking-at "[^|\n]*  |"))
    ;; There is room for insertion without re-aligning the table.
    (self-insert-command N)
    (org-table-with-shrunk-field
     (save-excursion
       (skip-chars-forward "^|")
       ;; Do not delete last space, which is
       ;; `org-table-separator-space', but the regular space before
       ;; it.
       (delete-region (- (point) 2) (1- (point))))))
   (t
    (setq org-table-may-need-update t)
    (self-insert-command N)
    (org-fix-tags-on-the-fly)
    (when org-self-insert-cluster-for-undo
      (if (not (eq last-command 'org-self-insert-command))
	  (setq org-self-insert-command-undo-counter 1)
	(if (>= org-self-insert-command-undo-counter 20)
	    (setq org-self-insert-command-undo-counter 1)
	  (and (> org-self-insert-command-undo-counter 0)
	       buffer-undo-list (listp buffer-undo-list)
	       (not (cadr buffer-undo-list)) ; remove nil entry
	       (setcdr buffer-undo-list (cddr buffer-undo-list)))
	  (setq org-self-insert-command-undo-counter
		(1+ org-self-insert-command-undo-counter))))))))

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

(provide 'shapes-module-org)
;;; shapes-org.el ends here
