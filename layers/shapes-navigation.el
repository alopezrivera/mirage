;; scrolling
(pixel-scroll-precision-mode)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil)

;; buffers
(global-set-key (kbd "C-<delete>") #'bury-buffer)

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
