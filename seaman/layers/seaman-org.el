;; settings
(setq-default org-use-property-inheritance t)

;; bindings
(global-set-key (kbd "C-x c") #'org-capture)

;; dependencies
(mirage-layer  'editing)

;; org
(mirage-module 'org)

;; editing
(mirage-module 'org-paragraph)
(mirage-module 'org-download)

;; templates
(mirage-module 'org-tempo)
(mirage-module 'org-capture)

;; agenda
(mirage-module 'org-agenda)
(mirage-module 'org-contacts)
(mirage-module 'org-calendar)
(mirage-extend 'org-agenda)

;; extensions
(mirage-extend 'org-get)
(mirage-extend 'org-queries)
(mirage-extend 'org-editing)
(mirage-extend 'org-ui)
(mirage-extend 'org-outline)
(mirage-extend 'org-navigation)

(provide 'mirage-layer-org)
;;; mirage-org.el ends here
