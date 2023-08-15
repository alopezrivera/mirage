;; buffer backups
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 2)
(setq kept-old-versions 2)

;; file manager
(seaman-module 'dirvish)

;; extensions
(seaman-extend 'file-management)

(provide 'seaman-layer-file-management)
;;; seaman-file-management.el ends here
