;; org-contacts
(straight-use-package '(org-contacts :type git :host nil :repo "https://repo.or.cz/org-contacts.git"))
(require 'org-contacts)

(defvar seaman/org-capture-contacts "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:" "org-contacts template")

(add-to-list 'org-capture-templates
   `(("c" "contact" entry
      (file ,(nth 0 org-contacts-files))
      ,seaman/org-capture-contacts)))

(provide 'shapes-module-org-contacts)
;;; shapes-org-contacts.el ends here
