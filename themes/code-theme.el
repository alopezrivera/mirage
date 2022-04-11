(use-package autothemer)

(autothemer-deftheme
 code "A theme for coding."

 ((((class color) (min-colors #xFFFFFF)))

  ;; Define color palette
  (code-white      "WhiteSmoke")
  (code-white2     "#fdfdfd")
  (code-grey       "grey92")
  (code-grey2      "LightSteelBlue4")
  (code-grey3      "#283f55")
  (code-black      "grey20")

  (code-green      "#2b9020")
  (code-red        "red3")
  (code-darkred    "DarkRed")
  (code-lila       "#696ee9")
  (code-purple     "MediumPurple2")
  (code-blue       "#cfdaff")
  (code-darkblue   "MidnightBlue"))

 ;; Customize faces
 ((default                      (:foreground code-black :background code-white ))

  ;; Cursor and selection
  (cursor                       (:background code-darkblue ))
  (region                       (:background code-blue ))
  (secondary-selection          (:background code-blue ))
  (line-number-current-line     (:foreground code-red ))

  ;; Org Mode
  (org-document-title           (:background code-white :foreground code-black))
  (org-level-1                  (:background code-white :foreground code-purple))

  ;; Org Babel
  (org-block-begin-line         (:foreground code-black :background code-grey ))
  (org-block                    (:background code-white2 ))
  (org-block-end-line           (:foreground code-black :background code-grey ))

  ;; Minibuffer and modeline
  (mode-line                    (:foreground code-grey3 :background code-white2 ))
  (minibuffer-prompt            (:foreground code-darkblue :bold t ))

  ;; Visual cues
  (fringe                       (:background code-white ))
  (font-lock-warning-face       (:foreground code-red :bold t ))
  
  ;; Bultins
  (font-lock-builtin-face       (:foreground code-purple ))
  ;; Comments
  (font-lock-comment-face       (:foreground code-grey2 ))
  ;; Functions
  (font-lock-function-name-face (:foreground code-darkblue ))
  ;; Keywords
  (font-lock-keyword-face       (:foreground code-darkred ))
  ;; Strings
  (font-lock-string-face        (:foreground code-green ))
  ;; Types
  (font-lock-type-face          (:foreground code-lila ))
  ;; Constants
  (font-lock-constant-face      (:foreground code-black ))
  ;; Variables
  (font-lock-variable-name-face (:foreground code-black ))))

(provide-theme 'code)
