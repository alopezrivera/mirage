;; org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; org-agenda-files
(setq org-agenda-files (append org-agenda-files
			       `(,config-directory)))

;; Tag indentation
(setq org-tags-column 70)

;; Mark items as done
(defun custom/org-agenda-todo-done ()
  (interactive)
  (org-agenda-todo 'done))

(define-key org-agenda-mode-map (kbd "d") 'custom/org-agenda-todo-done)

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	      ((agenda "" ((org-deadline-warning-days 7)))
	       (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
	       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	     ("n" "Next Tasks"
	      ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))

	     ("w" "Work Tasks" tags-todo "work")

	     ("e" "Emacs Tasks" tags-todo "emacs")

	     ("z" "Low Effort" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	      ((org-agenda-overriding-header "Low Effort Tasks")
	       (org-agenda-max-todos 20)
	       (org-agenda-files org-agenda-files)))

	     ("s" "Workflow Status"
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
		      (org-agenda-files org-agenda-files)))))))

;; Org Agenda log mode
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-tag-alist
      '((:startgroup)
	;; Put mutually exclusive tags here
	(:endgroup)
	("errand"   . ?e)
	("home"     . ?h)
	("work"     . ?w)
	("agenda"   . ?a)
	("planning" . ?p)
	("publish"  . ?P)
	("batch"    . ?b)
	("note"     . ?n)
	("idea"     . ?i)))

;; Define TODO keyword sequences
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)")
	    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(r)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(provide 'shapes-org-agenda)
;;; shapes-org-agenda.el ends here
