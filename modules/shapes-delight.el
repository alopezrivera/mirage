;; Customize names displayed in mode line
(straight-use-package 'delight)
(require 'delight)

;; Remove default modes from mode line
(delight '((global-command-log-mode nil "")
	      (olivetti-mode           nil "")
	      (which-key-mode          nil "")
	      (visual-line-mode        nil "simple")
	      (buffer-face-mode        nil "simple")
	      (org-indent-mode         nil "org")
	      (eldoc-mode              nil "eldoc")
	      ;; Major modes
	      (emacs-lisp-mode "EL" :major)))

(provide 'shapes-module-delight)
;;; shapes-delight.el ends here
