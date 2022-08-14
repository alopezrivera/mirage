(straight-use-package 'god-mode)
(require 'god-mode)

;; god
(global-set-key (kbd "<escape>") #'god-mode-all)

;; local
(define-key god-local-mode-map (kbd "i") #'god-local-mode)

;; bindings
(define-key god-local-mode-map (kbd ".") #'repeat)
(define-key god-local-mode-map (kbd "]") #'forward-paragraph)
(define-key god-local-mode-map (kbd "[") #'backward-paragraph)

(provide 'shapes-modules-god-mode)
;;; shapes-god-mode.el ends here
