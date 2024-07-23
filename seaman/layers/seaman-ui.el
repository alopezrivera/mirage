;; icons
(mirage-module 'all-the-icons)

;; highlights
(mirage-module 'rainbow-mode)

;; centering
(mirage-module 'olivetti)

;; line numbers
(global-set-key (kbd "C-c l") #'display-line-numbers-mode)

;; size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width  . 70))

;; tabs
(tab-bar-mode -1)
;; scroll bar
(scroll-bar-mode -1)
;; toolbar
(tool-bar-mode -1)
;; tooltips
(tooltip-mode -1)
;; menu bar
(menu-bar-mode -1)

;; title
(setq-default frame-title-format '("%b"))

;; fringes
(set-fringe-mode nil)

;; balance
(global-set-key (kbd "C-x -") #'balance-windows)

;; split threshold
(setq split-width-threshold 70)

;; visible bell
(setq visible-bell t)

;; time
(display-time-mode t)

;; column numbers
(column-number-mode)

;; mode display
(mirage-module 'delight)

;; extensions
(mirage-extend 'ui)

(provide 'mirage-layer-ui)
;;; mirage-ui.el ends here
