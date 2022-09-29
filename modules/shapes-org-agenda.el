;; org-agenda
(require 'org-agenda)
(global-set-key (kbd "C-c a") #'org-agenda)

;; org-agenda files
(setq org-agenda-files (append org-agenda-files `(,config-directory)))

;; Tag indentation
(setq org-tags-column 70)

;; Mark items as done
(defun custom/org-agenda-todo-done ()
  (interactive)
  (org-agenda-todo 'done))

(define-key org-agenda-mode-map (kbd "d") 'custom/org-agenda-todo-done)

(provide 'shapes-module-org-agenda)
;;; shapes-org-agenda.el ends here
