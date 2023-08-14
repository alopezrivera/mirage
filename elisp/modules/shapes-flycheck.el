;; flycheck
(straight-use-package 'flycheck)
(require 'flycheck)

(add-hook 'prog-mode-hook #'flycheck-mode)

(provide 'shapes-module-flycheck)
;;; shapes-flycheck.el ends here
