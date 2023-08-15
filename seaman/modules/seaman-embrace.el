;; dependencies
(seaman-module 'expand-region)

;; embrace
(straight-use-package 'embrace)
(global-set-key (kbd "C-,") #'embrace-commander)

(add-hook 'org-mode-hook #'embrace-org-mode-hook)

(provide 'seaman-module-embrace)
;;; seaman-embrace.el ends here
