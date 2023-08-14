(defun seaman-load-component (type component)
  "Load a seaman COMPONENT of the given TYPE"
  (let ((component-name (symbol-name component)))
       (condition-case err
           (require (intern (concat "seaman-" type "-" component-name)) (concat user-emacs-directory "elisp/" type "s" "/seaman-" component-name ".el"))
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

(provide 'seaman-core-load)
;;; seaman-load.el ends here
