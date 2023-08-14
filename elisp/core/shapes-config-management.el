(shapes-module "async")

(defun seaman/shapes-tangle (org-file &optional quiet)
  "Asynchronously tangle an org file."
  (let ((init-tangle-start-time (current-time))
	(file (buffer-file-name))
	(async-quiet-switch "-q"))
    (async-start
     `(lambda ()
              (dolist (comp-dir (mapcar (lambda (dir) (concat ,user-emacs-directory "elisp/" dir)) '("core" "layers" "modules" "extensions")))
                (dolist (file (directory-files comp-dir t directory-files-no-dot-files-regexp))
                  (delete-file file)))
	      (require 'org)
	      (add-hook 'org-babel-post-tangle-hook
			(lambda ()
				(string-match "\\(^.*/elisp/\\)\\(.*\\)\\(/shapes-\\)\\(.*\\)\\(.el\\)" (buffer-file-name))
				(let ((component (match-string 4 (buffer-file-name)))
				      (comp-type (match-string 2 (buffer-file-name))))
				     (end-of-buffer)
				     (insert (concat
					      "\n"
					      "(provide 'shapes-" (if (string-match-p "^.*s" comp-type)
                                                                      (substring comp-type 0 -1)
                                                                    comp-type)
                                              "-" component ")\n"
					      ";;; shapes-" component ".el ends here"))
				     (save-buffer))))
		   (org-babel-tangle-file ,org-file))
     (unless quiet
       `(lambda (result)
		     (if result
			 (message "SUCCESS: %s successfully tangled (%.2fs)."
				  ,(file-name-nondirectory org-file)
				  (float-time (time-subtract (current-time)
							     ',init-tangle-start-time)))
		       (message "ERROR: %s tangling failed." ,org-file)))))))

(defun seaman/shapes-tangle-auto ()
  "Automatically tangle Org Mode files in the Emacs config directory"
  (let* ((file   (expand-file-name buffer-file-name))
	 (source (string-match (concat user-emacs-directory ".*.org$") file))
	 (shapes (string-match (concat user-emacs-directory "shapes.org$") buffer-file-name))
	 (org-confirm-babel-evaluate nil))
    (if source
	(if shapes
	    (seaman/shapes-tangle file)
	  (org-babel-tangle)))))

(add-hook 'after-save-hook #'seaman/shapes-tangle-auto)

(shapes-module "no-littering")

(shapes-module "el-patch")

(global-set-key (kbd "C-x e") #'eval-buffer)

(global-set-key (kbd "C-c SPC") #'whitespace-mode)

(setq debug-on-error nil)

(provide 'shapes-core-config-management)
;;; shapes-config-management.el ends here
