(defcustom mirage/external-application-map (make-keymap)
  "External application keymap")

(defcustom mirage/external-application-bindings '()
  "List of external application key-binding pairs")

(defun mirage/bind-external-application (binding)
  (add-to-list 'mirage/external-application-bindings binding))

(global-set-key (kbd "C-c C-o") mirage/external-application-map)

(defun open-in-codium ()
  (interactive)
  (let ((root (projectile-root-bottom-up (file-name-directory buffer-file-name))))
    (call-process-shell-command (concat "codium " root "&") nil 0)))

(mirage/bind-external-application '("c" . open-in-codium))

(defun open-in-firefox ()
  (interactive)
  (call-process-shell-command (concat "firefox-trunk \"" (buffer-file-name) "\"") nil 0))

(mirage/bind-external-application '("f" . open-in-firefox))

(dolist (binding mirage/external-application-bindings)
  (define-key mirage/external-application-map (kbd (car binding)) (cdr binding)))

(provide 'mirage-extension-external-programs)
;;; mirage-external-programs.el ends here
