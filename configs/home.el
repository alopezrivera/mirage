(shapes-layer "ui")

(shapes-layer "input")

(setq light   'modus-operandi)
(setq dark    'modus-vivendi)
(shapes-layer "themes")

(setq modeline 'doom-modeline-mode)

(shapes-layer "mode-line")

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

(shapes-module "swiper")
(shapes-module "rg")

;; templates
(shapes-module "yasnippet")

;; completion
(shapes-module "ivy")

(shapes-module "desktop")
(shapes-module "workgroups")

(shapes-module "projectile")
(shapes-module "treemacs")

(shapes-module "counsel")
(shapes-module "helpful")
(shapes-module "which-key")

(shapes-layer "navigation")

(shapes-module "magit")

(shapes-layer "file-management")

(shapes-layer "ide")

(shapes-layer "latex")

(shapes-layer "org")
(shapes-layer "org-ui")
(shapes-layer "org-agenda")
(shapes-layer "org-latex-preview")
