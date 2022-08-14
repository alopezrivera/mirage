;; remove duplicates in shell history
(setq comint-input-ignoredups t)

;; folding
(shapes-module "hideshow")
;; completion
(shapes-module "company")
;; syntax checking
(shapes-module "flycheck")

;; lisp
(shapes-module "rainbow-delimiters")
;; python
(shapes-module "elpy")

(provide 'shapes-layer-ide)
;;; shapes-ide.el ends here
