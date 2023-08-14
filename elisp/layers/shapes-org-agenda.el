;; settings
(setq org-agenda-skip-scheduled-if-done nil)
(setq org-agenda-skip-deadline-if-done nil)
(setq org-agenda-window-setup 'current-window)
(setq org-deadline-warning-days 14)

;; bindings
(define-key org-agenda-mode-map (kbd "<tab>") 'org-agenda-recenter)

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

;; hide group tags
(setq org-agenda-hide-tags-regexp
      "CW\\|INT\\|THESIS\\|TA\\|BIRTHDAY\\|PERSONAL\\|PROFESSIONAL\\|TRAVEL\\|PEOPLE\\|HOME\\|FINANCE\\|PURCHASES\\|GIFTS")

(setq org-super-agenda-groups
      '(;; Each group has an implicit boolean OR operator between its selectors.
        (:name "Important"
               ;; Single arguments given alone
               :priority "A")
        (:name "Coursework"
               ;; Single arguments given alone
               :tag "CW")
        (:name "Internship"
               ;; Single arguments given alone
               :tag "INT")
        (:name "Thesis"
               ;; Single arguments given alone
               :tag "THESIS")
        (:name "Assistantships"
               ;; Single arguments given alone
               :tag "TA")
        (:name "Personal"
               ;; Single arguments given alone
               :tag "PERSONAL")
        (:name "Professional"
               ;; Single arguments given alone
               :tag "PROFESSIONAL")
        (:name "Travel"
               ;; Single arguments given alone
               :tag "TRAVEL")
        (:name "Keeping in touch"
               ;; Single arguments given alone
               :tag "PEOPLE")
        (:name "Home"
               ;; Single arguments given alone
               :tag "HOME")
        (:name "Medical"
               ;; Single arguments given alone
               :tag "MEDICAL")
        (:name "Finance"
               ;; Single arguments given alone
               :tag "FINANCE")
        (:name "Purchases"
               ;; Single arguments given alone
               :tag "PURCHASES")
        (:name "Gifts"
               ;; Single arguments given alone
               :tag "GIFTS")
        (:name "Birthdays"
               ;; Single arguments given alone
               :tag "BIRTHDAY")
        (:priority<= "B"
                     ;; Show this section after "Today" and "Important", because
                     ;; their order is unspecified, defaulting to 0. Sections
                     ;; are displayed lowest-number-first.
                     :order 1)
        ;; After the last group, the agenda will display items that didn't
        ;; match any of these groups, with the default order position of 99
        ))

(provide 'shapes-layer-org-agenda)
;;; shapes-org-agenda.el ends here
