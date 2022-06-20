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

(defvar config "home")

(defvar config-directory "~/.emacs.d/")

(defvar initial-buffer-choice "")

(defvar startup-buffers '())

(let ((startup (concat config-directory "local.el")))
  (if (file-exists-p startup)
      (load-file startup)))

(require (intern config) (concat config-directory (concat config ".el")))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c414f69a02b719fb9867b41915cb49c853489930be280ce81385ff7b327b4bf6" "02fff7eedb18d38b8fd09a419c579570673840672da45b77fde401d8708dc6b5" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
