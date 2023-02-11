(straight-use-package 'org-super-agenda)
(require 'org-super-agenda)

;; advice
(advice-add 'org-agenda :after #'org-super-agenda-mode)

(provide 'shapes-module-org-super-agenda)
;;; shapes-org-super-agenda.el ends here
