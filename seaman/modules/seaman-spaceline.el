;; spaceline
(straight-use-package 'spaceline)
(require 'spaceline-config)

;; mode line initialization hook
(add-hook 'after-init-hook #'spaceline-emacs-theme)

(provide 'mirage-module-spaceline)
;;; mirage-spaceline.el ends here
