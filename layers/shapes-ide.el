;; remove duplicates in shell history
(setq comint-input-ignoredups t)
;; indentation
(setq-default c-basic-offset 4)

;; outline
(shapes-module "hideshow")
;; editing
(shapes-module "puni")
(shapes-module "embrace")
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
