;; dependencies
(mirage-module 'expand-region)

;; embrace
(straight-use-package 'embrace)
(global-set-key (kbd "C-,") #'embrace-commander)

(add-hook 'org-mode-hook #'embrace-org-mode-hook)

(provide 'mirage-module-embrace)
;;; mirage-embrace.el ends here
