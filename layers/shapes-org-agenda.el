;; settings
(setq org-agenda-skip-scheduled-if-done nil)
(setq org-agenda-skip-deadline-if-done nil)
(setq org-agenda-window-setup 'current-window)
(setq org-deadline-warning-days 14)

;; modules
(shapes-module "org-agenda")
(shapes-module "org-super-agenda")
(shapes-module "org-rainbow-tags")

;; base TODO keyword sequence
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)")))

;; base custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Daily Dashboard"
	 ((agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 4)))
	  (todo "TODO" ((org-agenda-overriding-header "Unscheduled Tasks")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'scheduled 'deadline))))))
        ("w" "Weekly Dashboard"
	 ((agenda "" ((org-deadline-warning-days 14)))
	  (todo "TODO" ((org-agenda-overriding-header "Unscheduled Tasks")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'scheduled 'deadline))))))
        ("b" "Birthdays"
         ((org-super-agenda-mode -1)
          (agenda "" ((org-agenda-ndays 7))))
         ((org-agenda-regexp-filter-preset '("Birthday"))))))

(provide 'shapes-layer-org-agenda)
;;; shapes-org-agenda.el ends here
