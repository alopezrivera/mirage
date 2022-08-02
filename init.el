;; local settings
(let ((local (concat config-directory "local/local--" (system-name) ".el")))
  (if (file-exists-p local)
      (load-file local)
    (write-region ";; local emacs config" nil local)))

(defun shapes-load (dir src)
  "Load a shapeshifter configuration file located inside DIR within
 `config-directory', by name"
  (load-file (concat config-directory dir "/shapes-" src ".el")))

;; shapeshifter modules
(defun shapes-module (module)
  "Load a shapeshifter MODULE by name"
  (shapes-load "modules" module))

;; shapeshifter layers
(defun shapes-layer (layer)
  "Load a shapeshifter LAYER by name"
  (shapes-load "layers" layer))

;; shapeshifter extensions
(defun shapes-extend (extension)
  "Load a shapeshifter EXTENSION by name"
  (shapes-load "extensions" extension))

;; load config
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
