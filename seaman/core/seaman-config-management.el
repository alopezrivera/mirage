(defun mirage-load-component (type component)
  "Load a mirage COMPONENT of the given TYPE"
  (let ((component-name (symbol-name component)))
       (condition-case err
           (require (intern (concat "mirage-" type "-" component-name)) (concat user-emacs-directory "mirage/" type "s" "/mirage-" component-name ".el"))
         (error (progn (message "ERROR: mirage-%s %s load failed" component-name type)
		       (if debug-on-error
		           (debug err)))))))

;; mirage modules
(defun mirage-module (module)
  "Load a mirage MODULE by name"
  (mirage-load-component "module" module))

;; mirage layers
(defun mirage-layer (layer)
  "Load a mirage LAYER by name"
  (mirage-load-component "layer" layer))

;; mirage extensions
(defun mirage-extend (extension)
  "Load a mirage EXTENSION by name"
  (mirage-load-component "extension" extension))

;;;; Load async package
(straight-use-package 'async)
(require 'async)

;;;; Tangling machinery
(defun mirage/tangle (org-file &optional quiet)
  "Asynchronously tangle an org file."
  ;; Record initial time, name of current buffer and set async quiet switch
  (let ((init-tangle-start-time (current-time))
	(file (buffer-file-name))
	(async-quiet-switch "-q"))
    ;;; Async start
    (async-start
     ;;; Tangling function
     `(lambda ()
        ;; Delete all pre-existing Mirage components
        (dolist (comp-dir (mapcar (lambda (dir) (concat ,user-emacs-directory "mirage/" dir)) '("core" "layers" "modules" "extensions")))
          (dolist (file (directory-files comp-dir t directory-files-no-dot-files-regexp))
            (delete-file file)))
        ;; Require org-mode
	(require 'org)
        ;; Create a new org-mode tangling hook
	(add-hook 'org-babel-post-tangle-hook
		  (lambda ()
                    ;; Obtain component and component type from the name of the source file being tangled to
                    (if (string-match "\\(^.*/mirage/\\)\\(.*\\)\\(/mirage-\\)\\(.*\\)\\(.el\\)" (buffer-file-name))
		        (let ((component (match-string 4 (buffer-file-name)))
			      (comp-type (match-string 2 (buffer-file-name))))
		          (end-of-buffer)
		          (insert (concat
			           "\n"
			           "(provide 'mirage-" (if (string-match-p "^.*s" comp-type)
                                                           (substring comp-type 0 -1)
                                                         comp-type)
                                   "-" component ")\n"
			           ";;; mirage-" component ".el ends here"))
		          (save-buffer)))))
        ;; Tangle current file
	(org-babel-tangle-file ,org-file))
     ;;; Report success and tangling time (or failure)
     (unless quiet
       `(lambda (result)
	  (if result
	      (message "|mirage| Mirage components successfully tangled (%.2fs)."
		       (float-time (time-subtract (current-time) ',init-tangle-start-time)))
	    (message "|mirage| Mirage component tangling failed.")))))))

(defun mirage/tangle-auto ()
  "Automatically tangle Org Mode files in the Emacs config directory"
  (let* ((file   (expand-file-name buffer-file-name))
	 (config (string-match (concat user-emacs-directory "config.org$") file))
	 (mirage (string-match (concat user-emacs-directory "mirage/mirage.org$") buffer-file-name))
	 (org-confirm-babel-evaluate nil))
    (if mirage (mirage/tangle file))
    (if config (org-babel-tangle))))

(add-hook 'after-save-hook #'mirage/tangle-auto)

;;;; no-littering
(straight-use-package 'no-littering)
(require 'no-littering)

;;;; el-patch
(straight-use-package 'el-patch)
(require 'el-patch)

(setq debug-on-error nil)

(global-set-key (kbd "C-c SPC") #'whitespace-mode)

(provide 'mirage-core-config-management)
;;; mirage-config-management.el ends here
