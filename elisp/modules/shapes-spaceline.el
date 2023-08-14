;; spaceline
(straight-use-package 'spaceline)
(require 'spaceline-config)

;; mode line initialization hook
(add-hook 'after-init-hook #'spaceline-emacs-theme)

(provide 'shapes-module-spaceline)
;;; shapes-spaceline.el ends here
