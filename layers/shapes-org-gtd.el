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

(shapes-module "org-pomodoro")

(provide 'shapes-layer-org-gtd)
;;; shapes-org-gtd.el ends here
