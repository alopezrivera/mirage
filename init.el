(shapes-module "ef-themes")

(setq light-theme 'ef-deuteranopia-light)
(setq dark-theme  'ef-tritanopia-dark)

(shapes-layer "modeline-moody")

;; default
(set-face-attribute 'default nil        :font "Fira Code Retina" :height 93)

;; fixed pitch
(set-face-attribute 'fixed-pitch nil    :font "Fira Code Retina" :height 93)

;; variable pitch
(set-face-attribute 'variable-pitch nil :font "PT Sans"  :height 105 :weight 'regular)

;; italic
(defface custom/italic
  '((t :font "Victor Mono" :height  86 :weight  bold :slant italic))
  "Italic typeface")

;; titles
(setq typeface-title "Latin Modern Roman")

;; headings
(setq typeface-heading "Century Gothic")

;; mode line
(set-face-attribute 'mode-line nil :height 85 :inherit 'fixed-pitch)

(shapes-layer "editing")

(shapes-layer "search")

(shapes-module "yasnippet")

(shapes-layer "completion-vertico")

(shapes-layer "session")

(shapes-layer "project-interaction")

(shapes-module "counsel")
(shapes-module "helpful")
(shapes-module "which-key")

(shapes-layer "navigation")

(shapes-layer "version-control")

(shapes-layer "file-management")

(shapes-layer "ide")

(shapes-layer "pdf")

(shapes-layer "latex")

(shapes-layer "org-agenda")
(shapes-layer "org-gtd")

;; language
(setq default-input-method 'spanish-prefix)

;; custom link types
(@seaman/org-dir-link "msc1" (concat home "studio/academic/education/TU Delft/MSc/Space Flight/SPF-1/"))
(@seaman/org-dir-link "ta"(concat home "studio/academic/education/TU Delft/_assistantships/"))
