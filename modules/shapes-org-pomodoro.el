;; org-pomodoro
(straight-use-package 'org-pomodoro)

;; bindings
(define-key org-mode-map (kbd "C-c p") #'org-pomodoro)

;; visual notifications
(setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))

(provide 'shapes-module-org-pomodoro)
;;; shapes-org-pomodoro.el ends here
