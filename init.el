;; github
(global-set-key (kbd "C-M-g") (lambda () (interactive) (insert
"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")))

;; gitlab - DFKI
(global-set-key (kbd "C-M-p") (lambda () (interactive) (insert
"n~>}xr8AJ*?Y\"XG]")))

;; gitlab
(global-set-key (kbd "C-M-l") (lambda () (interactive) (insert
"n2hrBbaVZEE7b8k")))

;;; -*- lexical-binding: t; -*-

(defvar config "wild")

(defvar config-directory "~/.emacs.d/")

(defvar initial-buffer-choice "")

(defvar startup-buffers '())

;; local.el
(let ((local (concat config-directory "local.el")))
  (if (file-exists-p local)
      (load-file local)))

;; custom.el
(setq custom-file (concat config-directory "custom.el"))
(load custom-file)

(require (intern config) (concat config-directory (concat config ".el")))

(provide 'init)

