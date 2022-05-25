;; github
(global-set-key (kbd "C-M-g") (lambda () (interactive) (insert
"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")))

;; gitlab
(global-set-key (kbd "C-M-p") (lambda () (interactive) (insert
"n~>}xr8AJ*?Y\"XG]")))

;;; -*- lexical-binding: t; -*-

(defvar config-directory "~/.emacs.d/")

(defvar startup-buffers '())

(defvar initial-buffer-choice "")

(let ((startup (concat config-directory "startup.el")))
  (if (file-exists-p startup)
      (load-file startup)))

(require 'home (concat config-directory "home.el"))

;; (require 'wild (concat config-directory "wild.el"))

(provide 'init)
