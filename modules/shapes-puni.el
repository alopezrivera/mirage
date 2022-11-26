(straight-use-package 'puni)

;; mode hooks
(dolist (hook '(prog-mode-hook
                sgml-mode-hook
                nxml-mode-hook
                tex-mode-hook
                eval-expression-minibuffer-setup-hook))
  (add-hook hook #'puni-mode))

(provide 'shapes-module-puni)
;;; shapes-puni.el ends here
