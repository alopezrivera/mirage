;;;; Load async package
(straight-use-package 'async)
(require 'async)

;;;; Tangling machinery
(defun seaman/tangle (org-file &optional quiet)
  "Asynchronously tangle an org file."
  ;; Record initial time, name of current buffer and set async quiet switch
  (let ((init-tangle-start-time (current-time))
	(file (buffer-file-name))
	(async-quiet-switch "-q"))
    ;;; Async start
    (async-start
     ;;; Tangling function
     `(lambda ()
        ;; Delete all pre-existing Seaman components
        (dolist (comp-dir (mapcar (lambda (dir) (concat ,user-emacs-directory "elisp/" dir)) '("core" "layers" "modules" "extensions")))
          (dolist (file (directory-files comp-dir t directory-files-no-dot-files-regexp))
            (delete-file file)))
        ;; Require org-mode
	(require 'org)
        ;; Create a new org-mode tangling hook
	(add-hook 'org-babel-post-tangle-hook
		  (lambda ()
                    ;; Obtain component and component type from the name of the source file being tangled to
                    (if (string-match "\\(^.*/elisp/\\)\\(.*\\)\\(/seaman-\\)\\(.*\\)\\(.el\\)" (buffer-file-name))
		        (let ((component (match-string 4 (buffer-file-name)))
			      (comp-type (match-string 2 (buffer-file-name))))
		          (end-of-buffer)
		          (insert (concat
			           "\n"
			           "(provide 'seaman-" (if (string-match-p "^.*s" comp-type)
                                                           (substring comp-type 0 -1)
                                                         comp-type)
                                   "-" component ")\n"
			           ";;; seaman-" component ".el ends here"))
		          (save-buffer)))))
        ;; Tangle current file
	(org-babel-tangle-file ,org-file))
     ;;; Report success and tangling time (or failure)
     (unless quiet
       `(lambda (result)
	  (if result
	      (message "SUCCESS: %s successfully tangled (%.2fs)."
		       ,(file-name-nondirectory org-file)
		       (float-time (time-subtract (current-time)
						  ',init-tangle-start-time)))
	    (message "ERROR: %s tangling failed." ,org-file)))))))

(defun seaman/tangle-auto ()
  "Automatically tangle Org Mode files in the Emacs config directory"
  (let* ((file   (expand-file-name buffer-file-name))
	 (source (string-match (concat user-emacs-directory ".*.org$") file))
	 (seaman (string-match (concat user-emacs-directory "seaman.org$") buffer-file-name))
	 (org-confirm-babel-evaluate nil))
    (if source
	(if seaman
	    (seaman/tangle file)
	  (org-babel-tangle)))))

(add-hook 'after-save-hook #'seaman/tangle-auto)

;;;; no-littering
(straight-use-package 'no-littering)
(require 'no-littering)

;;;; el-patch
(straight-use-package 'el-patch)
(require 'el-patch)

(setq debug-on-error nil)

(global-set-key (kbd "C-c SPC") #'whitespace-mode)

(provide 'seaman-core-config-management)
;;; seaman-config-management.el ends here
