;; org-contacts
(straight-use-package
 '(org-contacts :type git :host nil :repo "https://repo.or.cz/org-contacts.git"))
(require 'org-contacts)

(global-set-key (kbd "C-x c") 'org-capture)

(defvar custom/org-capture-contacts "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:" "org-contacts template")

(setq org-capture-templates
   `(("c" "contact" entry
      (file ,(nth 0 org-contacts-files))
      ,custom/org-capture-contacts)))

(provide 'shapes-modules-org-contacts)
;;; shapes-org-contacts.el ends here
