;; elpy
(straight-use-package 'elpy)
(elpy-enable)

(setq elpy-rpc-timeout 5)

(setq elpy-rpc-backend "jedi")

(setq elpy-rpc-python-command "python3")

(define-key elpy-mode-map (kbd "C-M-n") 'elpy-nav-forward-block)
(define-key elpy-mode-map (kbd "C-M-p") 'elpy-nav-backward-block)

(provide 'mirage-module-elpy)
;;; mirage-elpy.el ends here
