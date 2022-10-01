(shapes-module "tablist")

(straight-use-package 'pdf-tools)
(pdf-tools-install)
(pdf-loader-install)

;; replace swiper
(define-key pdf-view-mode-map (kbd "C-s") #'isearch-forward)

;; page display size
(setq-default pdf-view-display-size 'fit-page)
;; automatically annotate highlights
(setq pdf-annot-activate-created-annotations t)

;; center
(define-key pdf-view-mode-map (kbd "c") #'pdf-view-center-in-window)
;; themed view
(define-key pdf-view-mode-map (kbd "C-c C-r t") #'pdf-view-themed-minor-mode)

(provide 'shapes-module-pdf-tools)
;;; shapes-pdf-tools.el ends here
