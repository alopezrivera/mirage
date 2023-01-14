;; file pre-processing
(let ((files org-agenda-files))
  (dolist (file files)
    (if (not (file-exists-p file))
        (progn (setq org-agenda-files (remove file files))
               (print (concat "WARNING: ignoring nonexistent agenda file: " file))))))

;; org-agenda
(require 'org-agenda)
(global-set-key (kbd "C-c a") #'org-agenda)

;; Tag indentation
(setq org-tags-column 70)

(provide 'shapes-module-org-agenda)
;;; shapes-org-agenda.el ends here
