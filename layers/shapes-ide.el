;; remove duplicates in shell history
(setq comint-input-ignoredups t)
;; indentation
(setq-default c-basic-offset 4)
;; electric pairs
(electric-pair-mode)
(setq electric-pair-pairs
      '(
        (?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})
        (?\" . ?\")))

;; outline
(shapes-module "hideshow")
;; editing
(shapes-module "puni")
(shapes-module "embrace")
;; completion
(shapes-module "company")
;; syntax checking
(shapes-module "flycheck")
;; language server protocol
(shapes-module "lsp-mode")

;; lisp
(shapes-module "rainbow-delimiters")
;; python
(shapes-module "elpy")
;; rust
(shapes-module "rustic")

(provide 'shapes-layer-ide)
;;; shapes-ide.el ends here
