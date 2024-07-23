;; doom-modeline
(straight-use-package 'doom-modeline)

;; bar
(setq-default doom-modeline-bar-width 0.01)

;; mode line initialization hook
(add-hook 'after-init-hook #'doom-modeline-mode)

(provide 'mirage-module-doom-modeline)
;;; mirage-doom-modeline.el ends here
