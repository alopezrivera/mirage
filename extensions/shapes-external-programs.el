(defcustom custom/external-application-map (make-keymap)
  "External application keymap")

(defcustom custom/external-application-bindings '()
  "List of external application key-binding pairs")

(defun custom/bind-external-application (binding)
  (add-to-list 'custom/external-application-bindings binding))

(global-set-key (kbd "C-c C-o") custom/external-application-map)

(defun open-in-codium ()
  (interactive)
  (let ((root (projectile-root-bottom-up (file-name-directory buffer-file-name))))
    (call-process-shell-command (concat "codium " root "&") nil 0)))

(custom/bind-external-application '("c" . open-in-codium))

(defun open-in-firefox ()
  (interactive)
  (call-process-shell-command (concat "firefox-trunk " (buffer-file-name)) nil 0))

(custom/bind-external-application '("f" . open-in-firefox))

(dolist (binding custom/external-application-bindings)
  (define-key custom/external-application-map (kbd (car binding)) (cdr binding)))

(provide 'shapes-extension-external-programs)
;;; shapes-external-programs.el ends here
