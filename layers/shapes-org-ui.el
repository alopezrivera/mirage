(shapes-module "org-modern")

(setq org-modern-table        nil)
(setq org-modern-block-fringe nil)

(shapes-module "org-appear")

;; title face
(defun custom/org-typeface-title ()
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-document-title        nil :font typeface-title   :weight 'regular :height 200 :foreground 'unspecified)))

(add-hook 'org-mode-hook #'custom/org-typeface-title)

(defun custom/org-typeface-indent ()
  "Indent typeface used in `org-indent-mode' and `visual-line-mode'."
  (with-eval-after-load 'org-indent-mode
    (set-face-attribute 'org-indent                nil                 :inherit '(org-hide fixed-pitch))))

(add-hook 'org-mode-hook #'custom/org-typeface-indent)

;; org-indent-mode
(setq org-startup-indented nil)

;; list indentation
(setq-default org-list-indent-offset 1)

;; startup with inline images
(setq org-startup-with-inline-images t)

;; no actual width
(setq org-image-actual-width nil)

;; do not consider empty lines content
(setq org-cycle-separator-lines 2)

(add-hook 'org-mode-hook (lambda () (progn (visual-line-mode 1) (setq line-move-visual t))))

(provide 'shapes-layer-org-ui)
;;; shapes-org-ui.el ends here
