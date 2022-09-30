(shapes-module "page-break-lines")
(shapes-module "projectile")
(shapes-module "all-the-icons")

(straight-use-package 'dashboard)
(require 'dashboard)

(dashboard-setup-startup-hook)

(setq dashboard-center-content t)

(provide 'shapes-module-emacs-dashboard)
;;; shapes-emacs-dashboard.el ends here
