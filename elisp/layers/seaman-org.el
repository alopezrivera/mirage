;; settings
(setq-default org-use-property-inheritance t)

;; bindings
(global-set-key (kbd "C-x c") #'org-capture)

;; dependencies
(seaman-layer  'editing)

;; org
(seaman-module 'org)

;; editing
(seaman-module 'org-paragraph)
(seaman-module 'org-download)

;; templates
(seaman-module 'org-tempo)
(seaman-module 'org-capture)

;; agenda - IMPORTANT: LOAD BEFORE org-roam
(seaman-module 'org-agenda)
(seaman-module 'org-contacts)
(seaman-module 'org-calendar)
(seaman-extend 'org-agenda)

;; notes
(seaman-module 'bitacora)
(seaman-module 'org-roam)

;; presentations
(seaman-module 'org-reveal)

;; programming
(seaman-module 'org-babel)
(seaman-module 'ox-ipynb)
(seaman-extend 'org-babel)

;; extensions
(seaman-extend 'org-get)
(seaman-extend 'org-queries)
(seaman-extend 'org-editing)
(seaman-extend 'org-ui)
(seaman-extend 'org-outline)
(seaman-extend 'org-navigation)

(provide 'seaman-layer-org)
;;; seaman-org.el ends here
