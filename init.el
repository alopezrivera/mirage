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
                   (lambda (component) (intern (file-name-sans-extension (file-name-nondirectory component))))
                   (file-expand-wildcards (concat config-directory "core/*.el")))))
  (mapc 'require components)
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
