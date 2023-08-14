(defun seaman/query-replace-regexp ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (call-interactively 'query-replace-regexp)))

(global-set-key (kbd "M-/") #'seaman/query-replace-regexp)

(provide 'shapes-extension-search)
;;; shapes-search.el ends here
