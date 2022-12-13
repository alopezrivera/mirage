;; requirements
(require 'projectile)
(require 'all-the-icons)
(require 'page-break-lines)

(straight-use-package 'dashboard)
(require 'dashboard)

(dashboard-setup-startup-hook)

;; options
(setq dashboard-center-content t)
(setq dashboard-set-file-icons t)
(setq dashboard-image-banner-max-width 300)
(setq dashboard-image-banner-max-height 300)

;; initial buffer choice
(if (and (not initial-buffer-choice)
         (string-equal (buffer-name (current-buffer)) "*scratch*"))
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(provide 'shapes-module-emacs-dashboard)
;;; shapes-emacs-dashboard.el ends here
