;;; -*- lexical-binding: t; -*-

;; display
(setq-default frame-title-format '("Emacs [%m] %b"))
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; typefaces
(set-face-attribute 'default     nil :height 85)

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

;; magit
(straight-use-package 'magit)

;; multiple cursors
(straight-use-package 'multiple-cursors)
(require 'multiple-cursors)

;; mc-lists
(setq mc/list-file "~/.emacs.d/mc-lists.el")

;; Create cursors
(global-set-key (kbd "C-.")         'mc/mark-next-like-this)
(global-set-key (kbd "C-;")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
(global-unset-key [C-down-mouse-1]) ; necessary

;; Return as usual
(define-key mc/keymap (kbd "<return>")       'electric-newline-and-maybe-indent)

;; Exit multiple-cursors-mode
(define-key mc/keymap (kbd "<escape>")       'multiple-cursors-mode)
(define-key mc/keymap (kbd "<mouse-1>")      'multiple-cursors-mode)
(define-key mc/keymap (kbd "<down-mouse-1>")  nil) ; necessary

;; swiper
(straight-use-package 'swiper)
(require 'swiper)

(defun custom/swiper-isearch (orig-fun &rest args)
  "`swiper-isearch' the selected region. If none are, `swiper-isearch'."
  (if (region-active-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(deactivate-mark)
	(apply orig-fun (buffer-substring-no-properties beg end)))
    (apply orig-fun args)))
(advice-add 'swiper-isearch :around #'custom/swiper-isearch)
(define-key global-map (kbd "C-s") #'swiper-isearch)

(defun custom/swiper-multiple-cursors ()
  (interactive)
  (swiper-mc)
  (minibuffer-keyboard-quit))
(define-key swiper-map (kbd "M-<return>") 'custom/swiper-multiple-cursors)

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
