;; ivy
(straight-use-package 'ivy)
(require 'ivy)

(ivy-mode 1)

;; minibuffer bindings
(let ((map ivy-minibuffer-map))
  (cl-loop for binding in '(("<tab>"       . ivy-alt-done)
			        ("<up>"        . ivy-previous-line-or-history)
			        ("C-l"         . ivy-alt-done)
			        ("C-j"         . ivy-next-line)
			        ("C-k"         . ivy-previous-line)
			        ("<backspace>" . ivy-backward-delete-char))
            collect (define-key map (kbd (car binding)) (cdr binding))))

;; switch-buffer bindings
(let ((map ivy-switch-buffer-map))
  (cl-loop for binding in '(("C-k"   . ivy-previous-line)
 			        ("C-l"   . ivy-done)
			        ("C-d"   . ivy-switch-buffer-kill))
            collect (define-key map (kbd (car binding)) (cdr binding))))

;; reverse-i-search bindings
(let ((map ivy-reverse-i-search-map))
  (cl-loop for binding in '(("C-k"   . ivy-previous-line)
			        ("C-d"   . ivy-reverse-i-search-kill))
            collect (define-key map (kbd (car binding)) (cdr binding))))

(provide 'shapes-modules-ivy)
;;; shapes-ivy.el ends here
