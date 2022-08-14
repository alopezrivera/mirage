;; projectile
(straight-use-package 'projectile)
(require 'projectile)

(projectile-mode)

;; command map prefix
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

(provide 'shapes-module-projectile)
;;; shapes-projectile.el ends here
