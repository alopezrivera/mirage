;; scrolling
(pixel-scroll-precision-mode)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil)

;; horizontal scrolling
(put 'scroll-left  'disabled nil)
(put 'scroll-right 'disabled nil)
;; mouse wheel
(global-set-key (kbd "S-<wheel-up>")    (lambda () (interactive) (scroll-right 2)))
(global-set-key (kbd "S-<wheel-down>")  (lambda () (interactive) (scroll-left  2)))
;; mouse side wheel
(global-set-key (kbd "S-<wheel-left>")  (lambda () (interactive) (scroll-right 4)))
(global-set-key (kbd "S-<wheel-right>") (lambda () (interactive) (scroll-left  4)))

;; buffers
(global-set-key (kbd "M-<delete>") #'bury-buffer)

;; windows
(winner-mode)

;; modules
(shapes-module "ace-window")

;; extensions
(shapes-extend "navigation")

;; bindings
(global-set-key (kbd "C-S-n") #'make-frame-command)

(provide 'shapes-layer-navigation)
;;; shapes-navigation.el ends here
