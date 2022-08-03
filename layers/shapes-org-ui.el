(shapes-module "org-modern")

(shapes-module "org-appear")

;; title face

(defun custom/org-typeface-title ()
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-document-title nil :font typeface-title :height 300 :weight 'regular :foreground 'unspecified)))

(add-hook 'org-mode-hook #'custom/org-typeface-title)

(defun custom/org-typefaces-body ()
  (with-eval-after-load 'org-faces

    ;; Code
    (set-face-attribute 'org-block                 nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code                  nil                 :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim              nil                 :inherit '(shadow fixed-pitch))

    ;; Tables
    (set-face-attribute 'org-table                 nil                 :inherit '(shadow fixed-pitch))

    ;; Lists
    (set-face-attribute 'org-checkbox              nil                 :inherit 'fixed-pitch)

    ;; Meta
    (set-face-attribute 'org-meta-line             nil                 :inherit 'fixed-pitch)
    (set-face-attribute 'org-document-info         nil                 :inherit 'fixed-pitch)
    (set-face-attribute 'org-document-info-keyword nil                 :inherit 'fixed-pitch)
    (set-face-attribute 'org-special-keyword       nil                 :inherit 'fixed-pitch)))

(add-hook 'org-mode-hook #'custom/org-typefaces-body)

(defun custom/org-typeface-indent ()
  "Indent typeface used in `org-indent-mode' and `visual-line-mode'."
  (with-eval-after-load 'org-indent-mode
    (set-face-attribute 'org-indent                nil                 :inherit '(org-hide fixed-pitch))))

(add-hook 'org-mode-hook #'custom/org-typeface-indent)

;; use levels 1 through 8
(setq org-n-level-faces 8)

;; do not cycle header style after 8th level
(setq org-cycle-level-faces nil)

;; hide leading stars
(setq org-hide-leading-starts t)

;; font sizes
(defun custom/org-header-setup () 
  (with-eval-after-load 'org-faces

    ;; heading font sizes
    (dolist (face '((org-level-1 . 1.6)
                    (org-level-2 . 1.4)
                    (org-level-3 . 1.3)
                    (org-level-4 . 1.2)
                    (org-level-5 . 1.15)
                    (org-level-6 . 1.10)
                    (org-level-7 . 1.05)
                    (org-level-8 . 1.00)))
         (set-face-attribute (car face) nil :font typeface-heading :weight 'bold :height (cdr face)))))

(add-hook 'org-mode-hook #'custom/org-header-setup)

;; org-indent-mode
(add-hook 'org-mode-hook #'org-indent-mode)

;; list indentation
(setq-default org-list-indent-offset 1)

(add-hook 'org-mode-hook (lambda () (progn (visual-line-mode 1) (setq line-move-visual t))))

;; symbols, super- and subscripts
(setq org-pretty-entities nil)

;; Change ellipsis ("...") to remove clutter
(setq org-ellipsis " ♢")

;; startup with inline images
(setq org-startup-with-inline-images t)

;; no actual width
(setq org-image-actual-width nil)

;; drag and drop
(shapes-module "org-download")
