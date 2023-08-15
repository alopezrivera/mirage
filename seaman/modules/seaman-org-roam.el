;; org-roam
(straight-use-package 'org-roam)

;; node visit hook
(defvar seaman/org-roam-node-visit-hook nil
   "Hook ran after `org-roam-node-visit'.")

(defun seaman/run-org-roam-node-visit-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'seaman/org-roam-node-visit-hook))

;; enable-theme
(advice-add 'org-roam-node-visit :after #'seaman/run-org-roam-node-visit-hook)

(if (and (boundp 'org-roam-directory) (file-directory-p org-roam-directory))
    (org-roam-db-autosync-mode))

(setq seaman/org-roam-map (make-keymap))
(global-set-key (kbd "C-r") seaman/org-roam-map)

;; Capture
(define-key seaman/org-roam-map (kbd "c") #'org-roam-capture)

;; Find node
(define-key seaman/org-roam-map (kbd "n") #'org-roam-node-find)

;; Insert reference
(define-key seaman/org-roam-map (kbd "i") #'org-roam-node-insert)

(provide 'seaman-module-org-roam)
;;; seaman-org-roam.el ends here
