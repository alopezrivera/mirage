;; Multiple cursors
(straight-use-package 'multiple-cursors)
(require 'multiple-cursors)

;; mc-lists
(setq mc/list-file (concat config-directory "persistent/mc-lists.el"))

;; Create cursors
(global-set-key (kbd "C-.")         #'mc/mark-next-like-this)
(global-set-key (kbd "C-;")         #'mc/mark-previous-like-this)
(global-set-key (kbd "C-<mouse-1>") #'mc/add-cursor-on-click)
(global-unset-key [C-down-mouse-1]) ; necessary

;; Return as usual
(define-key mc/keymap (kbd "<return>")       #'electric-newline-and-maybe-indent)

;; Exit multiple-cursors-mode
(define-key mc/keymap (kbd "<escape>")       #'multiple-cursors-mode)
(define-key mc/keymap (kbd "<mouse-1>")      #'multiple-cursors-mode)
(define-key mc/keymap (kbd "<down-mouse-1>")   nil) ; necessary

(provide 'shapes-multiple-cursors)
;;; shapes-multiple-cursors.el ends here
