;; commands
(shapes-module "god-mode")

;; confirmation
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; advanced commands
(put 'narrow-to-region 'disabled nil)

(provide 'shapes-layer-input)
;;; shapes-input.el ends here
