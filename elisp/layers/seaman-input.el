;; confirmation
(advice-add 'yes-or-no-p :override #'y-or-n-p)
;; advanced commands
(put 'narrow-to-region 'disabled nil)

;; modules
(seaman-module 'evil)
(seaman-module 'god-mode)

(provide 'seaman-layer-input)
;;; seaman-input.el ends here
