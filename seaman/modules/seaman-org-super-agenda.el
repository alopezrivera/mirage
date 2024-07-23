(straight-use-package 'org-super-agenda)
(require 'org-super-agenda)

;; advice
(advice-add 'org-agenda :before (lambda (&rest args) (org-super-agenda-mode 1)))

(provide 'mirage-module-org-super-agenda)
;;; mirage-org-super-agenda.el ends here
