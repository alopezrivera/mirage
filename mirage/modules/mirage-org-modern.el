(straight-use-package 'org-modern)

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(setq org-modern-list '((?+ . "-")
 		  	(?- . "•")
 			(?* . "▶")))

(setq org-modern-checkbox nil)

;; Vertical table line width
(setq org-modern-table-vertical 1)

;; Horizontal table line width
(setq org-modern-table-horizontal 1)

;; Tags
(setq org-modern-tag nil)

;; Priorities
(setq org-modern-priority nil)

(provide 'mirage-module-org-modern)
;;; mirage-org-modern.el ends here
