;; nano-modeline
(straight-use-package 'nano-modeline)

;; mode line initialization hook
(add-hook 'after-init-hook #'nano-modeline-mode)

(provide 'seaman-module-nano-modeline)
;;; seaman-nano-modeline.el ends here
