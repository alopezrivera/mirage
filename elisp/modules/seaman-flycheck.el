;; flycheck
(straight-use-package 'flycheck)
(require 'flycheck)

(add-hook 'prog-mode-hook #'flycheck-mode)

(provide 'seaman-module-flycheck)
;;; seaman-flycheck.el ends here
