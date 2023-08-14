;; doom-modeline
(straight-use-package 'doom-modeline)

;; bar
(setq-default doom-modeline-bar-width 0.01)

;; mode line initialization hook
(add-hook 'after-init-hook #'doom-modeline-mode)

(provide 'seaman-module-doom-modeline)
;;; seaman-doom-modeline.el ends here
