(straight-use-package 'puni)

;; specific modes
(dolist (hook '(prog-mode-hook
                tex-mode-hook
                eval-expression-minibuffer-setup-hook))
  (add-hook hook #'puni-mode))

(provide 'seaman-module-puni)
;;; seaman-puni.el ends here
