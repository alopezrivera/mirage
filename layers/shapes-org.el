;; dependencies
(shapes-layer  "editing")

;; org
(shapes-module "org")

;; editing
(shapes-module "org-paragraph")

;; templates
(shapes-module "org-tempo")
(shapes-module "org-capture")

;; notes
(shapes-module "bitacora")
(shapes-module "org-roam")

;; agenda
(shapes-module "org-agenda")
(shapes-module "org-contacts")
(shapes-module "org-calendar")

;; presentations
(shapes-module "org-reveal")

;; programming
(shapes-module "org-babel")

;; extensions
(shapes-extend "org-get")
(shapes-extend "org-queries")
(shapes-extend "org-editing")
(shapes-extend "org-display")
(shapes-extend "org-outline")
(shapes-extend "org-navigation")

(provide 'shapes-layer-org)
;;; shapes-org.el ends here
