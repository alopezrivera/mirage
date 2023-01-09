;; bindings
(global-set-key (kbd "C-x c") #'org-capture)

;; always use property inheritance
(setq-default org-use-property-inheritance t)

;; dependencies
(shapes-layer  "editing")

;; org
(shapes-module "org")

;; editing
(shapes-module "org-paragraph")
(shapes-module "org-download")

;; templates
(shapes-module "org-tempo")
(shapes-module "org-capture")

;; agenda - IMPORTANT: LOAD BEFORE org-roam
(shapes-module "org-agenda")
(shapes-module "org-contacts")
(shapes-module "org-calendar")
(shapes-extend "org-agenda")

;; notes
(shapes-module "bitacora")
(shapes-module "org-roam")

;; presentations
(shapes-module "org-reveal")

;; programming
(shapes-module "org-babel")
(shapes-module "ox-ipynb")
(shapes-extend "org-babel")

;; extensions
(shapes-extend "org-get")
(shapes-extend "org-queries")
(shapes-extend "org-editing")
(shapes-extend "org-ui")
(shapes-extend "org-outline")
(shapes-extend "org-navigation")

(provide 'shapes-layer-org)
;;; shapes-org.el ends here
