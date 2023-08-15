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
(seaman-module 'hideshow)
;; editing
(seaman-module 'puni)
(seaman-module 'embrace)
;; completion
(seaman-module 'company)
;; syntax checking
(seaman-module 'flycheck)
;; language server protocol
(seaman-module 'lsp-mode)

;; lisp
(seaman-module 'rainbow-delimiters)
;; python
(seaman-module 'elpy)
;; rust
(seaman-module 'rustic)

(provide 'seaman-layer-ide)
;;; seaman-ide.el ends here
