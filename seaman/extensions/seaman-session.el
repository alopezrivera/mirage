(defun mirage/load-session ()
  (interactive)
  (let ((session (read-file-name "Select session: "
                                 "~/sessions/"
                                 nil
                                 nil
                                 (car (directory-files "~/sessions/" nil "\\`[^.]*\\'")))))
    (desktop-change-dir session)
    (wg-load (concat session ".wg"))
    (call-interactively 'wg-switch-to-workgroup)))

(global-set-key (kbd "C-x s") #'mirage/load-session)

(provide 'mirage-extension-session)
;;; mirage-session.el ends here
