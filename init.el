;; github
(global-set-key (kbd "C-M-g") (lambda () (interactive) (insert
"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")))

;; gitlab
(global-set-key (kbd "C-M-p") (lambda () (interactive) (insert
"n~>}xr8AJ*?Y\"XG]")))

;;; -*- lexical-binding: t; -*-

(defvar config "home")

(defvar config-directory "~/.emacs.d/")

(defvar initial-buffer-choice "")

(defvar startup-buffers '())

(let ((startup (concat config-directory "local.el")))
  (if (file-exists-p startup)
      (load-file startup)))

(require (intern config) (concat config-directory (concat config ".el")))

(provide 'init)
