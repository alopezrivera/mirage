;; centering
(straight-use-package 'olivetti)

(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 0.9)))

;; normal modes
(dolist (mode '(org-mode-hook
		    magit-mode-hook
		    shell-mode-hook
		    markdown-mode-hook))
  (add-hook mode 'olivetti-mode))

;; Programming modes
(add-hook 'prog-mode-hook 'olivetti-mode)

(provide 'shapes-module-olivetti)
;;; shapes-olivetti.el ends here
