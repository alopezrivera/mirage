;; buffer backups
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 2)
(setq kept-old-versions 2)

;; file manager
(shapes-module "dirvish")

;; extensions
(shapes-extend "file-management")

(provide 'shapes-layers-file-management)
;;; shapes-file-management.el ends here
