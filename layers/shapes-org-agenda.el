;; file pre-processing
(let ((files org-agenda-files))
  (dolist (file files)
    (if (not (file-exists-p file))
        (progn (setq org-agenda-files (remove file files))
               (print (concat "WARNING: ignoring nonexistent agenda file: " file))))))

;; settings
(setq org-agenda-skip-scheduled-if-done nil)
(setq org-agenda-skip-deadline-if-done nil)
(setq org-agenda-window-setup 'reorganize-frame)
(setq org-deadline-warning-days 14)

;; modules
(shapes-module "org-agenda")
(shapes-module "org-super-agenda")
(shapes-module "org-rainbow-tags")

;; base TODO keyword sequence
(setq org-todo-keywords
      '((sequence "TODO(t)" "TODAY(n)" "NEXT(x)" "WAIT(w@/!)" "|" "DONE(d!)")))

;; base custom agenda views
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

(provide 'shapes-layer-org-agenda)
;;; shapes-org-agenda.el ends here
