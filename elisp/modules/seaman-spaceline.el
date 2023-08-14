;; spaceline
(straight-use-package 'spaceline)
(require 'spaceline-config)

;; mode line initialization hook
(add-hook 'after-init-hook #'spaceline-emacs-theme)

(provide 'seaman-module-spaceline)
;;; seaman-spaceline.el ends here
