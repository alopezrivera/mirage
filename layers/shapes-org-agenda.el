;; settings
(setq org-agenda-skip-scheduled-if-done nil)
(setq org-agenda-skip-deadline-if-done nil)
(setq org-agenda-window-setup 'reorganize-frame)
(setq org-deadline-warning-days 14)

;; modules
(shapes-module "org-agenda")
(shapes-module "org-super-agenda")
(shapes-module "org-rainbow-tags")

;; Define TODO keyword sequences
(setq org-todo-keywords
      '((sequence "TODO(t)" "TODAY(n)" "NEXT(x)" "WAIT(w@/!)" "|" "DONE(d!)")))

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "TODAY" ((org-agenda-overriding-header "Today")))
	  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	("n" "Tasks for the Day"
	 ((todo "TODAY" ((org-agenda-overriding-header "Today")))))

	("w" "Work Tasks" tags-todo "work")

	("z" "Low Effort" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	 ((org-agenda-overriding-header "Low Effort Tasks")
	  (org-agenda-max-todos 20)
	  (org-agenda-files org-agenda-files)))))

(add-to-list 'org-todo-keywords
             '(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(r)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)") t)

(add-to-list 'org-agenda-custom-commands
             '("s" "Workflow Status"
	       ((todo "WAIT"
		      ((org-agenda-overriding-header "Waiting on External")
		       (org-agenda-files org-agenda-files)))
	        (todo "REVIEW"
		      ((org-agenda-overriding-header "In Review")
		       (org-agenda-files org-agenda-files)))
	        (todo "PLAN"
		      ((org-agenda-overriding-header "In Planning")
		       (org-agenda-todo-list-sublevels nil)
		       (org-agenda-files org-agenda-files)))
	        (todo "BACKLOG"
		      ((org-agenda-overriding-header "Project Backlog")
		       (org-agenda-todo-list-sublevels nil)
		       (org-agenda-files org-agenda-files)))
	        (todo "READY"
		      ((org-agenda-overriding-header "Ready for Work")
		       (org-agenda-files org-agenda-files)))
	        (todo "ACTIVE"
		      ((org-agenda-overriding-header "Active Projects")
		       (org-agenda-files org-agenda-files)))
	        (todo "COMPLETED"
		      ((org-agenda-overriding-header "Completed Projects")
		       (org-agenda-files org-agenda-files)))
	        (todo "CANC"
		      ((org-agenda-overriding-header "Cancelled Projects")
		       (org-agenda-files org-agenda-files)))))
             t)

(provide 'shapes-layer-org-agenda)
;;; shapes-org-agenda.el ends here
