;; utilities
(mirage-extend 'get)
(mirage-extend 'queries)
(mirage-extend 'execution)
(mirage-extend 'operators)

;; UI
(mirage-layer 'ui)
(mirage-layer 'themes)
(mirage-layer 'dashboard)

;; input
(mirage-layer 'input)

;; org-mode
(mirage-layer 'org)
(mirage-layer 'org-ui)
(mirage-layer 'org-inline)
(mirage-layer 'org-typesetting)
(mirage-layer 'org-export)

;; org applications
(mirage-layer 'org-notebooks)
(mirage-layer 'org-zettelkasten)
(mirage-layer 'org-agenda)

;; PDFs
(mirage-layer 'pdf)

;; magit
(mirage-layer 'version-control)

(provide 'mirage-core-base-config)
;;; mirage-base-config.el ends here
