;; icons
(shapes-module "all-the-icons")

;; highlights
(shapes-module "rainbow-mode")

;; centering
(shapes-module "olivetti")

;; line numbers
(global-set-key (kbd "C-c l") #'display-line-numbers-mode)

;; size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width  . 70))

;; tabs
(tab-bar-mode 1)

;; no visible scroll bar
(scroll-bar-mode -1)
;; no toolbar
(tool-bar-mode -1)
;; no tooltips
(tooltip-mode -1)
;; no menu bar
(menu-bar-mode -1)

;; title
(setq-default frame-title-format '("Emacs [%m] %b"))

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
(shapes-module "delight")

;; extensions
(shapes-extend "ui")

(provide 'shapes-layer-ui)
;;; shapes-ui.el ends here
