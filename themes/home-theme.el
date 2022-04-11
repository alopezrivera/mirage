(use-package autothemer)

(autothemer-deftheme
 code "A theme for coding."

 ((((class color) (min-colors #xFFFFFF)))

  ;; Define color palette
  (home-white      "WhiteSmoke")
  (home-white2     "#fdfdfd")
  (home-grey       "grey92")
  (home-grey2      "LightSteelBlue4")
  (home-grey3      "#283f55")
  (home-black      "grey20")

  (home-green      "#2b9020")
  (home-red        "red3")
  (home-darkred    "DarkRed")
  (home-lila       "#696ee9")
  (home-purple     "MediumPurple2")
  (home-blue       "#cfdaff")
  (home-darkblue   "MidnightBlue"))

 ;; Customize faces
 ((default                      (:foreground home-white :background home-black ))
  ;; Current
  (cursor                       (:background home-darkblue ))
  (region                       (:background home-blue ))
  (secondary-selection          (:background home-blue ))
  (line-number-current-line     (:foreground home-red ))

  ;; Org Babel
  (org-block-begin-line         (:foreground home-black :background home-grey ))
  (org-block                    (:background home-white2 ))
  (org-block-end-line           (:foreground home-black :background home-grey ))

  ;; Minibuffer and modeline
  (mode-line                    (:foreground home-grey3 :background home-white2 ))
  (minibuffer-prompt            (:foreground home-darkblue :bold t ))

  ;; Visual cues
  (fringe                       (:background home-white ))
  (font-lock-warning-face       (:foreground home-red :bold t ))
  
  ;; Bultins
  (font-lock-builtin-face       (:foreground home-purple ))
  ;; Comments
  (font-lock-comment-face       (:foreground home-grey2 ))
  ;; Functions
  (font-lock-function-name-face (:foreground home-darkblue ))
  ;; Keywords
  (font-lock-keyword-face       (:foreground home-darkred ))
  ;; Strings
  (font-lock-string-face        (:foreground home-green ))
  ;; Types
  (font-lock-type-face          (:foreground home-lila ))
  ;; Constants
  (font-lock-constant-face      (:foreground home-black ))
  ;; Variables
  (font-lock-variable-name-face (:foreground home-black ))))

(provide-theme 'code)
