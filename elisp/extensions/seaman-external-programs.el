(defcustom seaman/external-application-map (make-keymap)
  "External application keymap")

(defcustom seaman/external-application-bindings '()
  "List of external application key-binding pairs")

(defun seaman/bind-external-application (binding)
  (add-to-list 'seaman/external-application-bindings binding))

(global-set-key (kbd "C-c C-o") seaman/external-application-map)

(defun open-in-codium ()
  (interactive)
  (let ((root (projectile-root-bottom-up (file-name-directory buffer-file-name))))
    (call-process-shell-command (concat "codium " root "&") nil 0)))

(seaman/bind-external-application '("c" . open-in-codium))

(defun open-in-firefox ()
  (interactive)
  (call-process-shell-command (concat "firefox-trunk \"" (buffer-file-name) "\"") nil 0))

(seaman/bind-external-application '("f" . open-in-firefox))

(dolist (binding seaman/external-application-bindings)
  (define-key seaman/external-application-map (kbd (car binding)) (cdr binding)))

(provide 'seaman-extension-external-programs)
;;; seaman-external-programs.el ends here
