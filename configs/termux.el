;;; -*- lexical-binding: t; -*-

;;; Commentary
;; Emacs configuration for Termux

;;; Code


(setq inhibit-startup-message t)

;; infrastructure
(shapes-module "straight")

;; functions
(shapes-module "magit")
(global-set-key (kbd "C-p") (kbd "C-M-g"))

(shapes-module "el-patch")
(shapes-extend "general")
(shapes-module "org")
(shapes-module "multiple-cursors")
(shapes-extend "editing")
(shapes-extend "org")
(require 'org-diary (concat config-directory "packages/org-diary.el"))
(require 'org-paragraph (concat config-directory "packages/org-paragraph.el"))

;; command
(shapes-module "god-mode")
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; completion
(shapes-module "ivy")
(shapes-module "swiper")

;; ui
(menu-bar-mode -1)
(global-set-key (kbd "C-c l") #'display-line-numbers-mode)

(shapes-layer "themes")
(shapes-layer "mode-line")


(provide 'termux)
;;; termux.el ends here
