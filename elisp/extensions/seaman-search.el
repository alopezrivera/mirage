(defun seaman/query-replace-regexp ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (call-interactively 'query-replace-regexp)))

(global-set-key (kbd "M-/") #'seaman/query-replace-regexp)

(provide 'seaman-extension-search)
;;; seaman-search.el ends here
