;; org-pomodoro
(straight-use-package 'org-pomodoro)

;; bindings
(define-key org-mode-map (kbd "C-c p") #'org-pomodoro)

;; visual notifications
(setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))

(provide 'mirage-module-org-pomodoro)
;;; mirage-org-pomodoro.el ends here
