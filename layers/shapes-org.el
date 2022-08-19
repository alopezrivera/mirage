(shapes-module "org")

;; editing
(shapes-module "org-paragraph")

;; templates
(shapes-module "org-tempo")
(shapes-module "org-capture")

;; export
(shapes-module "ox-rst")

;; notes
(shapes-module "org-diary")
(shapes-module "org-roam")

;; agenda
(shapes-module "org-agenda")
(shapes-module "org-contacts")
(shapes-module "org-calendar")

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
