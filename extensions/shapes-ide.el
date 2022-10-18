(defun custom/codium ()
  (interactive)
  (let ((root (projectile-root-bottom-up (file-name-directory buffer-file-name))))
    (call-process-shell-command (concat "codium " root "&") nil 0)))

(define-key projectile-command-map (kbd "o") #'custom/codium)

(provide 'shapes-extension-ide)
;;; shapes-ide.el ends here
