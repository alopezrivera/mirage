;; org-contacts
(straight-use-package '(org-contacts :type git :host nil :repo "https://repo.or.cz/org-contacts.git"))
(require 'org-contacts)

(defvar mirage/org-capture-contacts "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:" "org-contacts template")

(add-to-list 'org-capture-templates
   `(("c" "contact" entry
      (file ,(nth 0 org-contacts-files))
      ,mirage/org-capture-contacts)))

(provide 'mirage-module-org-contacts)
;;; mirage-org-contacts.el ends here
