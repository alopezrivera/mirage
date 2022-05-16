(global-set-key (kbd "C-M-p") (lambda () (interactive) (insert
"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")))

;;; -*- lexical-binding: t; -*-

(setq config-directory "~/.emacs.d/")

(require 'home (concat config-directory "home.el"))

;; (require 'wild (concat config-directory "wild.el"))

(provide 'init)
