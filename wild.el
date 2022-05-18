;;; -*- lexical-binding: t; -*-

;; display
(setq-default frame-title-format '("Emacs [%m] %b"))
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; dialogues
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; background buffers
(defvar custom/background-buffers
  '("~/.emacs.d/wild.el"))

(defun custom/spawn-startup-buffers ()
  (cl-loop for buffer in (append custom/startup-buffers custom/background-buffers)
	   collect (find-file-noselect buffer)))

(add-hook 'after-init-hook #'custom/spawn-startup-buffers)

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

;; winner
(winner-mode)

;; workgroups
(straight-use-package 'workgroups)
(require 'workgroups)

(setq wg-prefix-key (kbd "C-c g"))

(workgroups-mode 1)

;; ide
(require 'ide (concat config-directory "ide.el"))

;; declare
(provide 'wild)
