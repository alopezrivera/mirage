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

(provide 'seaman-module-org-roam-timestamps)
;;; seaman-org-roam-timestamps.el ends here
