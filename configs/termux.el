;;; -*- lexical-binding: t; -*-

;;; Commentary
;; Emacs configuration for Termux

;;; Code


(setq inhibit-startup-message t)

;; infrastructure
(shapes-module "straight")

;; functions
(shapes-module "magit")
(global-set-key (kbd "C-p") (lambda () (interactive) (insert "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")))

(shapes-layer "org")
(shapes-layer "org-ui")

(define-key org-mode-map (kbd "RET") #'custom/org-return)

(require 'org-diary (concat config-directory "packages/org-diary.el"))
(require 'org-paragraph (concat config-directory "packages/org-paragraph.el"))

;; command
(shapes-module "god-mode")
(global-set-key (kbd "C-c g") #'god-mode-all)
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; completion
(shapes-module "ivy")
(shapes-module "swiper")

;; ui
(menu-bar-mode -1)
(global-set-key (kbd "C-c l") #'display-line-numbers-mode)

(setq light 'chocolate)
(shapes-layer "themes")
(shapes-layer "mode-line")


(provide 'termux)
;;; termux.el ends here
