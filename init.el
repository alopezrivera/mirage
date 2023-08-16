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
(defface seaman/italic
  '((t :font "Victor Mono" :height  86 :weight  bold :slant italic))
  "Italic typeface")

;; titles
(setq typeface-title "Latin Modern Roman")

;; headings
(setq typeface-heading "Century Gothic")

;; mode line
(set-face-attribute 'mode-line nil :height 85 :inherit 'fixed-pitch)

(seaman-module 'counsel)
(seaman-module 'helpful)
(seaman-module 'which-key)

(seaman-layer 'navigation)

;; text editing
(seaman-layer 'editing)
;; text search
(seaman-layer 'search)
;; snippets
(seaman-module 'yasnippet)
;; auto-completions
(seaman-layer 'completion-vertico)

;; session management
(seaman-layer 'session)
;; project management
(seaman-layer 'project-interaction)

(seaman-layer 'file-management)

(seaman-layer 'ide)

(seaman-layer 'latex)

(seaman-module 'bitacora)

(seaman-layer 'org-agenda)
(seaman-layer 'org-gtd)

;; language
(setq default-input-method 'spanish-prefix)

;; custom link types
(@seaman/org-dir-link "msc1" (concat home "studio/academic/education/TU Delft/MSc/Space Flight/SPF-1/"))
(@seaman/org-dir-link "ta"(concat home "studio/academic/education/TU Delft/_assistantships/"))

;; org-roam capture templates
(setq org-roam-capture-templates
      '(("m" "mathematics" plain "%?"
         :target (file+head "mathematics/%<%Y%m%d%H%M%S>-${slug}.org"
			           "#+STARTUP: subtree\n\n\n\n#+title:${title}\n\n\n")
         :unnarrowed t)
        ("c" "control" plain "%?"
         :target (file+head "control/%<%Y%m%d%H%M%S>-${slug}.org"
			           "#+STARTUP: subtree\n\n\n\n#+title:${title}\n\n\n")
         :unnarrowed t)))

;; org-agenda setup
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
