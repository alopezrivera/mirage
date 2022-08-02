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

;; shapeshifter outfits
(defun shapes-outfit (outfit)
  "Load a shapeshifter OUTFIT by name"
  (shapes-load "outfits" outfit))

;; shapeshifter extensions
(defun shapes-extend (extension)
  "Load a shapeshifter EXTENSION by name"
  (shapes-load "extensions" extension))

;; load config
(require (intern config) (concat config-directory "configs/" (concat config ".el")))

;; background buffers
(defvar background-buffers
  (list (concat config-directory "local.el")
        (concat config-directory "init.org")
        ;; configs
        (concat config-directory "home.org")
        (concat config-directory "wild.el")
        ;; modules
        (concat config-directory "modules/org.org")
        (concat config-directory "modules/ide.org")
        (concat config-directory "modules/ui.org")
        (concat config-directory "modules/theme.org")
        ;; packages
        (concat config-directory "packages/org-diary.org")
        (concat config-directory "packages/org-paragraph.org")

        (concat config-directory "demo.org")
        (concat config-directory "system.org")
        (concat config-directory "dotfiles.org")
        (concat config-directory "backlog.org")))

(defvar spawn-startup-buffers t
  "Whether to spawn spawn the buffers in the `startup-buffers' list after initialization")

(defvar spawn-background-buffers nil
  "Whether to spawn spawn the buffers in the `background-buffers' list after initialization")
  
  (defun custom/spawn-buffers (buffer-list)
    "Spawn buffers in buffer list"
    (cl-loop for buffer in buffer-list
	     collect (find-file-noselect buffer)))

  (defun custom/spawn-startup-buffers ()
    (custom/spawn-buffers startup-buffers))

  (defun custom/spawn-background-buffers ()
    (custom/spawn-buffers background-buffers))

  (if spawn-startup-buffers
      (add-hook 'after-init-hook #'custom/spawn-startup-buffers))

  (if spawn-background-buffers
      (add-hook 'after-init-hook #'custom/spawn-background-buffers))

;; credentials
(condition-case nil
    (load-file (concat config-directory "creds.el.gpg"))
  (error nil))

(provide 'init)
