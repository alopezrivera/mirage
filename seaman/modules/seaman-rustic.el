(straight-use-package 'rustic)

;; Do not format org-babel blocks after a successful build
(setq rustic-babel-format-src-block nil)
;; Do not display compilation buffer of babel process
(setq rustic-babel-display-compilation-buffer nil)

;; Add cargo to exec-path
(add-to-list 'exec-path "~/.cargo/bin")

(provide 'seaman-module-rustic)
;;; seaman-rustic.el ends here
