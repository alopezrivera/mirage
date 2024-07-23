;; remove duplicates in shell history
(setq comint-input-ignoredups t)
;; indentation
(setq-default c-basic-offset 4)
;; electric pairs
(setq electric-pair-pairs
      '((?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})
        (?\" . ?\")))
(electric-pair-mode)

;; outline
(mirage-module 'hideshow)
;; editing
(mirage-module 'puni)
(mirage-module 'embrace)
;; completion
(mirage-module 'company)
;; syntax checking
(mirage-module 'flycheck)
;; language server protocol
(mirage-module 'lsp-mode)

;; lisp
(mirage-module 'rainbow-delimiters)
;; python
(mirage-module 'elpy)
;; rust
(mirage-module 'rustic)

(provide 'mirage-layer-ide)
;;; mirage-ide.el ends here
