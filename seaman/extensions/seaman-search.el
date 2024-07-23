(defun mirage/query-replace-regexp ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (call-interactively 'query-replace-regexp)))

(global-set-key (kbd "M-/") #'mirage/query-replace-regexp)

(provide 'mirage-extension-search)
;;; mirage-search.el ends here
