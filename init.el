;; local settings
(let ((local (concat config-directory "local/local--" (system-name) ".el")))
  (if (file-exists-p local)
      (load-file local)
    (write-region ";; local emacs config" nil local)))

;; config directory
(setq config-directory (string-replace "~" (getenv "HOME") config-directory))

;; shapes core
(add-to-list 'load-path (concat config-directory "core/"))

(let ((components (mapcar
		   (lambda (source)
		     (string-match "\\(^.*/shapes-\\)\\(.*\\)\\(.el\\)" source)
		     (match-string 2 source))
		   (file-expand-wildcards (concat config-directory "core/*.el")))))
  (mapc (lambda (component) (require (intern (concat "shapes-core-" component)) (concat "shapes-" component))) components)
  (message "Shapes: core loaded"))

(load-file (concat config-directory "configs/" (concat config ".el")))

;; inhibit startup message
(setq inhibit-startup-message t)

;; startup buffers
(defun custom/spawn-startup-buffers ()
  "Spawn startup buffers"
  (cl-loop for buffer in startup-buffers
	      collect (find-file-noselect buffer)))

(if spawn-startup-buffers
    (add-hook 'after-init-hook #'custom/spawn-startup-buffers))

;; credentials
(condition-case nil
    (load-file (concat config-directory "creds.el.gpg"))
  (error nil))

(provide 'init)
