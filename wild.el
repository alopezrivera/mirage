;;; -*- lexical-binding: t; -*-

(setq config-directory "~/.emacs.d/")
(setq startup-buffers  '("~/.emacs.d/wild.el"))

(global-set-key (kbd "C-M-p") (lambda () (interactive) (insert
"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")))

;; display
(setq-default frame-title-format '("Emacs [%m] %b"))
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; dialogues
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; startup buffers
(dolist (startup-b startup-buffers)
  (find-file-noselect startup-b))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; modus
(straight-use-package 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-operandi)

;; selectrum
(straight-use-package 'selectrum)
(selectrum-mode +1)

;; magit
(straight-use-package 'magit)

;; ide
(require 'ide (concat config-directory "ide.el"))

;; declare
(provide 'wild)
