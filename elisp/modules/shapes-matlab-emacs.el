(straight-use-package '(matlab-emacs :type git :host nil :repo "https://git.code.sf.net/p/matlab-emacs/src"))
(load-library "matlab-load")

(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))

(provide 'shapes-module-matlab-emacs)
;;; shapes-matlab-emacs.el ends here
