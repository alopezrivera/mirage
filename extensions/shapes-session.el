(defun custom/desktop-change-dir ()
  (interactive)
  (desktop-change-dir (read-file-name "Select session: " "~/sessions/")))

(global-set-key (kbd "C-x s") #'custom/desktop-change-dir)

(provide 'shapes-extension-session)
;;; shapes-session.el ends here
