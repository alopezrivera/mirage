(shapes-layer "ui")

(setq default-input-method 'spanish-prefix)

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

;; custom link types
(@custom/org-dir-link "msc1" (concat home "studio/academic/education/TU Delft/MSc/Space Flight/SPF-1/"))
(@custom/org-dir-link "ta"(concat home "studio/academic/education/TU Delft/_assistantships/"))

;; org-agenda
(shapes-layer "org-agenda")

;; hide group tags
(setq org-agenda-hide-tags-regexp
      "CW\\|INT\\|THESIS\\|TA\\|BIRTHDAY\\|PERSONAL\\|PROFESSIONAL\\|TRAVEL\\|PEOPLE\\|HOME\\|FINANCE\\|PURCHASES\\|GIFTS")

(setq org-super-agenda-groups
      '(;; Each group has an implicit boolean OR operator between its selectors.
        (:name "Important"
               ;; Single arguments given alone
               :priority "A")
        (:name "Coursework"
               ;; Single arguments given alone
               :tag "CW")
        (:name "Internship"
               ;; Single arguments given alone
               :tag "INT")
        (:name "Thesis"
               ;; Single arguments given alone
               :tag "THESIS")
        (:name "Assistantships"
               ;; Single arguments given alone
               :tag "TA")
        (:name "Personal"
               ;; Single arguments given alone
               :tag "PERSONAL")
        (:name "Professional"
               ;; Single arguments given alone
               :tag "PROFESSIONAL")
        (:name "Travel"
               ;; Single arguments given alone
               :tag "TRAVEL")
        (:name "Keeping in touch"
               ;; Single arguments given alone
               :tag "PEOPLE")
        (:name "Home"
               ;; Single arguments given alone
               :tag "HOME")
        (:name "Medical"
               ;; Single arguments given alone
               :tag "MEDICAL")
        (:name "Finance"
               ;; Single arguments given alone
               :tag "FINANCE")
        (:name "Purchases"
               ;; Single arguments given alone
               :tag "PURCHASES")
        (:name "Gifts"
               ;; Single arguments given alone
               :tag "GIFTS")
        (:name "Birthdays"
               ;; Single arguments given alone
               :tag "BIRTHDAY")
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
