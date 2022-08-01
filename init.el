;;; -*- lexical-binding: t; -*-

(defcustom config "home"
  "Emacs configuration of choice")

(defcustom config-directory "~/.emacs.d/"
  "Emacs configuration directory")

(defcustom initial-buffer-choice ""
  "Buffer displayed at startup")

(defcustom startup-buffers '()
  "Buffers opened at startup")

;; customize interface file
(setq custom-file (concat config-directory "persistent/custom.el"))
(load-file custom-file)

(provide 'early-init)

;; local settings
(let ((local (concat config-directory "local/local--" (system-name) ".el")))
  (if (file-exists-p local)
      (load-file local)
    (write-region ";; local emacs config" nil local)))

;; load config
(require (intern config) (concat config-directory "configs/" (concat config ".el")))

;; credentials
(load-file (concat config-directory "creds.el.gpg"))

(provide 'init)
