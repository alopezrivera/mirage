;; org-roam
(straight-use-package 'org-roam)

;; Org Roam UI
(straight-use-package 'org-roam-ui)

(setq org-roam-ui-follow t)

;; Sync theme and UI
(setq org-roam-ui-sync-theme nil)

(setq org-roam-ui-open-on-start nil)

(setq org-roam-ui-update-on-save t)

;; Node visit hook
(defvar custom/org-roam-node-visit-hook nil
   "Hook ran after `org-roam-node-visit'.")

(defun custom/run-org-roam-node-visit-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'custom/org-roam-node-visit-hook))

;; enable-theme
(advice-add 'org-roam-node-visit :after #'custom/run-org-roam-node-visit-hook)

(if (and (boundp 'org-roam-directory) (file-directory-p org-roam-directory))
    (org-roam-db-autosync-mode))

(add-hook 'org-roam-find-file-hook #'variable-pitch-mode)

;; Find node
(global-set-key (kbd "C-c n") 'org-roam-node-find)

;; Insert reference
(global-set-key (kbd "C-c i") 'org-roam-node-insert)

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
	    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			       "#+STARTUP: subtree\n\n\n\n#+title:${title}\n\n\n")
	    :unnarrowed t)))

;; org-roam-timestamps
(straight-use-package 'org-roam-timestamps)
(require 'org-roam-timestamps)

;; remember
(setq org-roam-timestamps-remember-timestamps nil)
(setq org-roam-timestamps-minimum-gap 3600)

;; visit hook
(add-hook 'custom/org-roam-node-visit-hook #'org-roam-timestamps-mode)

;; capture hook
(defvar custom/org-roam-timestamps-mode-active-before-capture nil)

(defun custom/org-roam-timestamps-mode-off ()
  "Disable `org-roam-timestamps-mode' in Org Roam capture buffers."
  (setq custom/org-roam-timestamps-mode-active-before-capture org-roam-timestamps-mode)
  (org-roam-timestamps-mode -1))
(add-hook 'org-roam-capture-new-node-hook #'custom/org-roam-timestamps-mode-off)

(defun custom/org-roam-timestamps-mode-back ()
  "Re-enable `org-roam-timestamps-mode' after finalizing capture,
if it was previously enabled."
  (if custom/org-roam-timestamps-mode-active-before-capture
      (org-roam-timestamps-mode)))
(add-hook 'org-capture-after-finalize-hook #'custom/org-roam-timestamps-mode-back)

(provide 'shapes-org-roam)
;;; shapes-org-roam.el ends here
