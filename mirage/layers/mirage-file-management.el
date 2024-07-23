;; buffer backups
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 2)
(setq kept-old-versions 2)

;; file manager
(mirage-module 'dirvish)

;; extensions
(mirage-extend 'file-management)

(provide 'mirage-layer-file-management)
;;; mirage-file-management.el ends here
