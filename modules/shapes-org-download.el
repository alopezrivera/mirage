(straight-use-package 'org-download)

(dolist (mode '(org-mode-hook
		     dired-mode-hook))
  (add-hook mode 'org-download-enable))

;; download directory
(setq-default org-download-image-dir "./figures")

;; customize #+DOWNLOADED attribute
(defun custom/org-download-annotate (link)
  "Create a captioned and labeled figure."
  (concat "#+CAPTION:\n"
          "#+NAME: fig:\n"))
(setq org-download-annotate-function #'custom/org-download-annotate)

(provide 'shapes-org-download)
;;; shapes-org-download.el ends here
