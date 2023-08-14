;; org-roam
(straight-use-package 'org-roam)

;; org-roam-ui
(straight-use-package 'org-roam-ui)

(setq org-roam-ui-follow t)

;; sync theme and ui
(setq org-roam-ui-sync-theme nil)

(setq org-roam-ui-open-on-start nil)

(setq org-roam-ui-update-on-save t)

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

(setq org-roam-capture-templates
      '(("m" "mathematics" plain "%?"
         :target (file+head "mathematics/%<%Y%m%d%H%M%S>-${slug}.org"
			           "#+STARTUP: subtree\n\n\n\n#+title:${title}\n\n\n")
         :unnarrowed t)
        ("c" "control" plain "%?"
         :target (file+head "control/%<%Y%m%d%H%M%S>-${slug}.org"
			           "#+STARTUP: subtree\n\n\n\n#+title:${title}\n\n\n")
         :unnarrowed t)))

;; org-roam-timestamps
(straight-use-package 'org-roam-timestamps)
(require 'org-roam-timestamps)

;; remember
(setq org-roam-timestamps-remember-timestamps nil)
(setq org-roam-timestamps-minimum-gap 3600)

;; visit hook
(add-hook 'seaman/org-roam-node-visit-hook #'org-roam-timestamps-mode)

;; capture hook
(defvar seaman/org-roam-timestamps-mode-active-before-capture nil)

(defun seaman/org-roam-timestamps-mode-off ()
  "Disable `org-roam-timestamps-mode' in Org Roam capture buffers."
  (setq seaman/org-roam-timestamps-mode-active-before-capture org-roam-timestamps-mode)
  (org-roam-timestamps-mode -1))
(add-hook 'org-roam-capture-new-node-hook #'seaman/org-roam-timestamps-mode-off)

(defun seaman/org-roam-timestamps-mode-back ()
  "Re-enable `org-roam-timestamps-mode' after finalizing capture,
if it was previously enabled."
  (if seaman/org-roam-timestamps-mode-active-before-capture
      (org-roam-timestamps-mode)))
(add-hook 'org-capture-after-finalize-hook #'seaman/org-roam-timestamps-mode-back)

(provide 'seaman-module-org-roam)
;;; seaman-org-roam.el ends here
