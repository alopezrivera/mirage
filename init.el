(global-set-key (kbd "C-M-p") (lambda () (interactive) (insert
"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")))

;;; -*- lexical-binding: t; -*-

(setq config-directory "~/.emacs.d/")

(defvar custom/startup-buffers
  '("/home/emacs/test.org"
    "/home/dfki/backlog.org" "/mnt/c/Users/xXY4n/Professional/_Employment/DFKI/Repos/hopping_leg/software/python/hopping_leg/motors/abstract_motor.py"))

(setq initial-buffer-choice "/home/dfki/backlog.org")

(require 'home (concat config-directory "home.el"))

;; (require 'wild (concat config-directory "wild.el"))

(provide 'init)
