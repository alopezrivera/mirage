;; confirmation
(advice-add 'yes-or-no-p :override #'y-or-n-p)
;; advanced commands
(put 'narrow-to-region 'disabled nil)

;; modules
(mirage-module 'evil)
(mirage-module 'god-mode)

;; extensions
(mirage-extend 'rsi)

(provide 'mirage-layer-input)
;;; mirage-input.el ends here
