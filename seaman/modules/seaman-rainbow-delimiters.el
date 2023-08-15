;; rainbow-delimieters
(straight-use-package 'rainbow-delimiters)
(require 'rainbow-delimiters)

;; enable rainbow delimiters on all programming modes
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(provide 'seaman-module-rainbow-delimiters)
;;; seaman-rainbow-delimiters.el ends here
