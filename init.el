(seaman-module 'ef-themes)

(setq light-theme 'ef-deuteranopia-light)
(setq dark-theme  'ef-tritanopia-dark)

(seaman-layer 'modeline-moody)

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

(seaman-layer 'editing)

(seaman-layer 'search)

(seaman-module 'yasnippet)

(seaman-layer 'completion-vertico)

(seaman-layer 'session)

(seaman-layer 'project-interaction)

(seaman-module 'counsel)
(seaman-module 'helpful)
(seaman-module 'which-key)

(seaman-layer 'navigation)

(seaman-layer 'version-control)

(seaman-layer 'file-management)

(seaman-layer 'ide)

(seaman-layer 'pdf)

(seaman-layer 'latex)

(seaman-layer 'org-agenda)
(seaman-layer 'org-gtd)

;; language
(setq default-input-method 'spanish-prefix)

;; custom link types
(@seaman/org-dir-link "msc1" (concat home "studio/academic/education/TU Delft/MSc/Space Flight/SPF-1/"))
(@seaman/org-dir-link "ta"(concat home "studio/academic/education/TU Delft/_assistantships/"))
