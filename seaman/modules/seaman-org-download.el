(straight-use-package 'org-download)

(dolist (mode '(org-mode-hook
		dired-mode-hook))
  (add-hook mode #'org-download-enable))

;; download directory
(setq-default org-download-image-dir "./figures")

;; customize #+DOWNLOADED attribute
(defun mirage/org-download-annotate (link)
  "Create a captioned and labeled figure."
  (concat "#+CAPTION:\n"
          "#+NAME: fig:\n"))
(setq org-download-annotate-function #'mirage/org-download-annotate)

(provide 'mirage-module-org-download)
;;; mirage-org-download.el ends here
