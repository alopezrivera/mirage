(straight-use-package 'nano-theme)
(require 'nano-theme)

;; always use load-theme + enable-theme for the nano themes
(advice-add 'enable-theme :around (lambda (orig-fun THEME) (if (string-match "^nano-.*" (symbol-name THEME))
                                                               (progn (load-theme THEME t t)
                                                                      (funcall orig-fun THEME))
                                                             (funcall orig-fun THEME))))

(provide 'mirage-module-nano-theme)
;;; mirage-nano-theme.el ends here
