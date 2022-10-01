(shapes-module "tablist")

(straight-use-package 'pdf-tools)
(pdf-tools-install)
(pdf-loader-install)

;; page display size
(setq-default pdf-view-display-size 'fit-page)
;; automatically annotate highlights
(setq pdf-annot-activate-created-annotations t)

;; replace swiper
(define-key pdf-view-mode-map (kbd "C-s") #'isearch-forward)
;; themed view
(define-key pdf-view-mode-map (kbd "C-c C-r t") #'pdf-view-themed-minor-mode)

(provide 'shapes-module-pdf-tools)
;;; shapes-pdf-tools.el ends here
