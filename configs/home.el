(shapes-layer "ui")

(shapes-layer "input")
(shapes-extend "rsi")

(setq light   'ef-deuteranopia-light)
(setq dark    'ef-tritanopia-dark)
(shapes-layer "themes")

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

;; templates
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

(shapes-layer "org")
(shapes-layer "org-ui")
(shapes-layer "org-typesetting")
(shapes-layer "org-latex-preview")

;; custom links
(@custom/org-dir-link "msc1" (concat home "studio/academic/education/TU Delft/MSc/Space Flight/SPF-1/"))

;; org-agenda
(shapes-layer "org-agenda")

(setq org-tag-alist
      '((:startgroup)
	;; Put mutually exclusive tags here
        ("internship" . ?u)
	(:endgroup)))

(setq org-super-agenda-groups
      '(;; Each group has an implicit boolean OR operator between its selectors.
        (:name "Today"  ; Optionally specify section name
               :time-grid t  ; Items that appear on the time grid
               :todo "TODAY")  ; Items that have this TODO keyword
        (:name "Important"
               ;; Single arguments given alone
               :priority "A")
        (:name "Birthdays"
               ;; Single arguments given alone
               :file-path ".*/contact book.org")
        (:priority<= "B"
                     ;; Show this section after "Today" and "Important", because
                     ;; their order is unspecified, defaulting to 0. Sections
                     ;; are displayed lowest-number-first.
                     :order 1)
        ;; After the last group, the agenda will display items that didn't
        ;; match any of these groups, with the default order position of 99
        ))

;; org-gtd
(shapes-layer "org-gtd")
