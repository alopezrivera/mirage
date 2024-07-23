;; org-roam-timestamps
(straight-use-package 'org-roam-timestamps)
(require 'org-roam-timestamps)

;; remember
(setq org-roam-timestamps-remember-timestamps nil)
(setq org-roam-timestamps-minimum-gap 3600)

;; visit hook
(add-hook 'mirage/org-roam-node-visit-hook #'org-roam-timestamps-mode)

;; capture hook
(defvar mirage/org-roam-timestamps-mode-active-before-capture nil)

(defun mirage/org-roam-timestamps-mode-off ()
  "Disable `org-roam-timestamps-mode' in Org Roam capture buffers."
  (setq mirage/org-roam-timestamps-mode-active-before-capture org-roam-timestamps-mode)
  (org-roam-timestamps-mode -1))
(add-hook 'org-roam-capture-new-node-hook #'mirage/org-roam-timestamps-mode-off)

(defun mirage/org-roam-timestamps-mode-back ()
  "Re-enable `org-roam-timestamps-mode' after finalizing capture,
if it was previously enabled."
  (if mirage/org-roam-timestamps-mode-active-before-capture
      (org-roam-timestamps-mode)))
(add-hook 'org-capture-after-finalize-hook #'mirage/org-roam-timestamps-mode-back)

(provide 'mirage-module-org-roam-timestamps)
;;; mirage-org-roam-timestamps.el ends here
