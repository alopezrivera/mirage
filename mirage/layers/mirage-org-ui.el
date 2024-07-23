;; org-indent-mode
(setq org-startup-indented nil)
;; tag indentation
(setq org-tags-column 0)
;; list indentation
(setq-default org-list-indent-offset 1)
;; startup with inline images
(setq org-startup-with-inline-images t)
;; do not force inline images to their actual width
(setq org-image-actual-width nil)
;; do not consider empty lines content
(setq org-cycle-separator-lines 2)

;; line wrapping
(add-hook 'org-mode-hook (lambda () (progn (visual-line-mode 1) (setq line-move-visual t))))

;; design
(mirage-module 'org-modern)
(setq org-modern-table        nil)
(setq org-modern-block-fringe nil)

(provide 'mirage-layer-org-ui)
;;; mirage-org-ui.el ends here
