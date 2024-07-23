(straight-use-package 'moody)

;; configuration
(setq x-underline-at-descent-line t)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
(moody-replace-eldoc-minibuffer-message-function)

;; reload active theme
(let ((active-theme (car custom-enabled-themes)))
  (if active-theme (enable-theme active-theme)))

(provide 'mirage-module-moody)
;;; mirage-moody.el ends here
