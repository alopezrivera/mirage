(straight-use-package 'workgroups)
(require 'workgroups)

(setq wg-prefix-key (kbd "C-c w"))

;; save commands
(define-key wg-map (kbd "s")   #'wg-save)
(define-key wg-map (kbd "C-s") #'wg-update-all-workgroups-and-save)

;; suppress animation
(setq wg-morph-on nil)

(global-set-key (kbd "C-c w") #'workgroups-mode)

(provide 'seaman-module-workgroups)
;;; seaman-workgroups.el ends here
