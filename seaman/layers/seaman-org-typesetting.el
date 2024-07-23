(defun mirage/org-typefaces-body ()
  (with-eval-after-load 'org-faces

    ;; Code
    (set-face-attribute 'org-block                 nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code                  nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim              nil :inherit '(shadow fixed-pitch))

    ;; Tables
    (set-face-attribute 'org-table                 nil :inherit '(shadow fixed-pitch))

    ;; Lists
    (set-face-attribute 'org-checkbox              nil :inherit 'fixed-pitch)

    ;; Meta
    (set-face-attribute 'org-meta-line             nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-document-info         nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-special-keyword       nil :inherit 'fixed-pitch)))

(add-hook 'org-mode-hook #'mirage/org-typefaces-body)

;; use levels 1 through 16
(setq org-n-level-faces 8)

;; do not cycle header style after 8th level
(setq org-cycle-level-faces t)

;; hide leading stars
(setq org-hide-leading-starts t)

;; font sizes
(defun mirage/org-heading-typefaces () 
  (with-eval-after-load 'org-faces
    (dolist (face '((org-level-1  . 1.175)
                    (org-level-2  . 1.175)
                    (org-level-3  . 1.175)
                    (org-level-4  . 1.175)
                    (org-level-5  . 1.175)
                    (org-level-6  . 1.175)
                    (org-level-7  . 1.175)
                    (org-level-8  . 1.175)))
         (set-face-attribute (car face) nil :font typeface-heading :weight 'bold :height (cdr face)))))

;; org-mode hook
(add-hook 'org-mode-hook #'mirage/org-heading-typefaces)

;; theme hooks
(add-hook 'mirage/enable-or-load-theme-hook #'mirage/org-typefaces-body)
(add-hook 'mirage/enable-or-load-theme-hook #'mirage/org-heading-typefaces)

;; symbols, super- and subscripts
(setq org-pretty-entities nil)

;; Change ellipsis ("...") to remove clutter
(setq org-ellipsis " ♢")

(provide 'mirage-layer-org-typesetting)
;;; mirage-org-typesetting.el ends here
