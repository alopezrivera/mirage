;; commands
(shapes-module "god-mode")

;; confirmation
(advice-add 'yes-or-no-p :override #'y-or-n-p)

(provide 'shapes-input)
;;; shapes-input.el ends here
