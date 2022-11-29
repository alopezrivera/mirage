(shapes-module "projectile")
(shapes-module "all-the-icons")
(shapes-module "page-break-lines")

(straight-use-package 'dashboard)
(require 'dashboard)

(dashboard-setup-startup-hook)

;; options
(setq dashboard-center-content t)
(setq dashboard-set-file-icons t)

;; initial buffer choice
(if (and (not initial-buffer-choice)
         (string-equal (buffer-name (current-buffer)) "*scratch*"))
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(provide 'shapes-module-emacs-dashboard)
;;; shapes-emacs-dashboard.el ends here
