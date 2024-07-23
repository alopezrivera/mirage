(defun mirage/org-get-title (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (nth 1
	   (assoc "TITLE"
		  (org-element-map (org-element-parse-buffer 'greater-element)
		      '(keyword)
		    #'mirage/get-keyword-key-value))))))

(defun mirage/org-get-file-title (file)
  (with-current-buffer (find-file-noselect file)
       (mirage/org-get-title)))

(defun mirage/org-get-subtree-region (&optional element)
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

(defun mirage/org-get-subtree-content ()
  "Retrieve the content of the current subtree."
  (setq content (apply #'buffer-substring-no-properties (mirage/org-get-subtree-region))))

(provide 'mirage-extension-org-get)
;;; mirage-org-get.el ends here
