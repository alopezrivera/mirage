(straight-use-package 'workgroups)
(require 'workgroups)

(setq wg-prefix-key (kbd "C-c w"))

;; save commands
(define-key wg-map (kbd "s")   #'wg-save)
(define-key wg-map (kbd "C-s") #'wg-update-all-workgroups-and-save)

;; suppress animation
(setq wg-morph-on nil)

(workgroups-mode 1)

(provide 'shapes-workgroups)
;;; shapes-workgroups.el ends here
