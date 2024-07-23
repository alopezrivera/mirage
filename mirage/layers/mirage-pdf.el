;; settings
(setq doc-view-resolution 250)

;; requirements
(mirage-module 'tablist)

;; modules
(mirage-module 'pdf-tools)
(mirage-module 'pdf-view-restore)

;; extensions
(mirage-extend 'pdf)

(provide 'mirage-layer-pdf)
;;; mirage-pdf.el ends here
