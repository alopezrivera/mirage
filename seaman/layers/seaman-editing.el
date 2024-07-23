;; settings
(setq-default indent-tabs-mode nil)

;; built-ins
(global-so-long-mode 1)

;; modules
(mirage-module 'vundo)
(mirage-module 'multiple-cursors)

;; extensions
(mirage-extend 'editing)

(provide 'mirage-layer-editing)
;;; mirage-editing.el ends here
