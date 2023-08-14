;; centering
(straight-use-package 'olivetti)

(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 0.9)))

;; normal modes
(dolist (mode '(org-mode-hook
		     shell-mode-hook
		     markdown-mode-hook
                latex-mode-hook))
  (add-hook mode 'olivetti-mode))

;; Programming modes
(add-hook 'prog-mode-hook #'olivetti-mode)

(require 'el-patch)

(el-patch-feature olivetti)
(el-patch-defun olivetti-set-width (width)
  "Set text body width to WIDTH with relative margins.
WIDTH may be an integer specifying columns or a float specifying
a fraction of the window width."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Set text body width (integer or float): "
                        olivetti-body-width))))
  (setq olivetti-body-width width)
  (olivetti-set-buffer-windows)
  (el-patch-remove (message "Text body width set to %s" olivetti-body-width)))

(provide 'shapes-module-olivetti)
;;; shapes-olivetti.el ends here
