;;; -*- lexical-binding: t; -*-

(setq config-directory "~/.emacs.d/")

;; display
(setq-default frame-title-format '("Emacs [%m] %b"))
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

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

;; selectrum
(straight-use-package 'selectrum)
(selectrum-mode +1)

;; magit
(straight-use-package 'magit)

;; modus
(use-package modus-themes)
(modus-themes-load-themes)
(modus-themes-load-operandi)

;; declare
(provide 'wild)
