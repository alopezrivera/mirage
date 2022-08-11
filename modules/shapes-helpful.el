;; replace description key bindings by their helpful equivalents
(straight-use-package 'helpful)

(setq counsel-describe-function-function  #'helpful-callable)
(setq counsel-describe-variable-function  #'helpful-variable)

(global-set-key [remap describe-function] #'helpful-function)
(global-set-key [remap describe-command]  #'helpful-command)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-key]      #'helpful-key)

(provide 'shapes-helpful)
;;; shapes-helpful.el ends here
