(defun seaman/load-session ()
  (interactive)
  (let ((session (read-file-name "Select session: "
                                 "~/sessions/"
                                 nil
                                 nil
                                 (car (directory-files "~/sessions/" nil "\\`[^.]*\\'")))))
    (desktop-change-dir session)
    (wg-load (concat session ".wg"))
    (call-interactively 'wg-switch-to-workgroup)))

(global-set-key (kbd "C-x s") #'seaman/load-session)

(provide 'shapes-extension-session)
;;; shapes-session.el ends here
