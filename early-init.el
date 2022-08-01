;;; -*- lexical-binding: t; -*-

(defcustom config                "home"
  "Emacs configuration of choice")

(defcustom config-directory      "~/.emacs.d/"
  "Emacs configuration directory")

(defcustom initial-buffer-choice ""
  "Buffer displayed at startup")

(defcustom startup-buffers       '()
  "Buffers opened at startup")

;; customize interface file
(setq custom-file (concat config-directory "persistent/custom.el"))
(load-file custom-file)

(provide 'early-init)
