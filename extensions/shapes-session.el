(defun custom/load-session ()
  (interactive)
  (let ((session (read-file-name "Select session: " "~/sessions/")))
    (desktop-change-dir session)
    (wg-load (concat session ".wg"))
    (call-interactively 'wg-switch-to-workgroup)))

(global-set-key (kbd "C-x s") #'custom/load-session)

(provide 'shapes-extension-session)
;;; shapes-session.el ends here
