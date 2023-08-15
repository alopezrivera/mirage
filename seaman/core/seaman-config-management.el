(defun seaman-load-component (type component)
  "Load a seaman COMPONENT of the given TYPE"
  (let ((component-name (symbol-name component)))
       (condition-case err
           (require (intern (concat "seaman-" type "-" component-name)) (concat user-emacs-directory "seaman/" type "s" "/seaman-" component-name ".el"))
         (error (progn (message "ERROR: seaman-%s %s load failed" component-name type)
		       (if debug-on-error
		           (debug err)))))))

;; seaman modules
(defun seaman-module (module)
  "Load a seaman MODULE by name"
  (seaman-load-component "module" module))

;; seaman layers
(defun seaman-layer (layer)
  "Load a seaman LAYER by name"
  (seaman-load-component "layer" layer))

;; seaman extensions
(defun seaman-extend (extension)
  "Load a seaman EXTENSION by name"
  (seaman-load-component "extension" extension))

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
        (dolist (comp-dir (mapcar (lambda (dir) (concat ,user-emacs-directory "seaman/" dir)) '("core" "layers" "modules" "extensions")))
          (dolist (file (directory-files comp-dir t directory-files-no-dot-files-regexp))
            (delete-file file)))
        ;; Require org-mode
	(require 'org)
        ;; Create a new org-mode tangling hook
	(add-hook 'org-babel-post-tangle-hook
		  (lambda ()
                    ;; Obtain component and component type from the name of the source file being tangled to
                    (if (string-match "\\(^.*/seaman/\\)\\(.*\\)\\(/seaman-\\)\\(.*\\)\\(.el\\)" (buffer-file-name))
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
	      (message "|SEAMAN| Seaman components successfully tangled (%.2fs)."
		       (float-time (time-subtract (current-time) ',init-tangle-start-time)))
	    (message "|SEAMAN| Seaman component tangling failed.")))))))

(defun seaman/tangle-auto ()
  "Automatically tangle Org Mode files in the Emacs config directory"
  (let* ((file   (expand-file-name buffer-file-name))
	 (config (string-match (concat user-emacs-directory "config.org$") file))
	 (seaman (string-match (concat user-emacs-directory "seaman/seaman.org$") buffer-file-name))
	 (org-confirm-babel-evaluate nil))
    (if seaman (seaman/tangle file))
    (if config (org-babel-tangle))))

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
