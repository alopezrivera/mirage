;; org-roam
(straight-use-package 'org-roam)

;; node visit hook
(defvar mirage/org-roam-node-visit-hook nil
   "Hook ran after `org-roam-node-visit'.")

(defun mirage/run-org-roam-node-visit-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'mirage/org-roam-node-visit-hook))

;; enable-theme
(advice-add 'org-roam-node-visit :after #'mirage/run-org-roam-node-visit-hook)

(if (and (boundp 'org-roam-directory) (file-directory-p org-roam-directory))
    (org-roam-db-autosync-mode))

(setq mirage/org-roam-map (make-keymap))
(global-set-key (kbd "C-r") mirage/org-roam-map)

;; Capture
(define-key mirage/org-roam-map (kbd "c") #'org-roam-capture)

;; Find node
(define-key mirage/org-roam-map (kbd "n") #'org-roam-node-find)

;; Insert reference
(define-key mirage/org-roam-map (kbd "i") #'org-roam-node-insert)

(provide 'mirage-module-org-roam)
;;; mirage-org-roam.el ends here
