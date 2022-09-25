;; confirmation
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; advanced commands
(put 'narrow-to-region 'disabled nil)

;; modules
(shapes-module "god-mode")

(provide 'shapes-layer-input)
;;; shapes-input.el ends here
