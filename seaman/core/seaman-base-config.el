;; utilities
(seaman-extend 'get)
(seaman-extend 'queries)
(seaman-extend 'execution)
(seaman-extend 'operators)

;; UI
(seaman-layer 'ui)
(seaman-layer 'themes)
(seaman-layer 'dashboard)

;; input
(seaman-layer 'input)

;; org-mode
(seaman-layer 'org)
(seaman-layer 'org-ui)
(seaman-layer 'org-inline)
(seaman-layer 'org-typesetting)
(seaman-layer 'org-export)

;; org applications
(seaman-layer 'org-notebooks)
(seaman-layer 'org-zettelkasten)
(seaman-layer 'org-agenda)

;; PDFs
(seaman-layer 'pdf)

;; magit
(seaman-layer 'version-control)

(provide 'seaman-core-base-config)
;;; seaman-base-config.el ends here
