;; scrolling
(pixel-scroll-precision-mode)
(setq auto-window-vscroll nil)

;; modules
(shapes-module "winner")
(shapes-module "ace-window")

;; extensions
(shapes-extend "navigation")

;; bindings
(global-set-key (kbd "C-S-n") #'make-frame-command)

(provide 'shapes-layer-navigation)
;;; shapes-navigation.el ends here
