;; flycheck
(straight-use-package 'flycheck)
(require 'flycheck)

(add-hook 'prog-mode-hook #'flycheck-mode)

(provide 'mirage-module-flycheck)
;;; mirage-flycheck.el ends here
