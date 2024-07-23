(defun mirage/modeline-set-colors (fg fg-in bg bg-in)
  "Set the color of the mode and header lines and blend the 
`doom-modeline-bar' with the background."
  (set-face-attribute 'mode-line            nil :foreground fg    :background bg    :box nil)
  (set-face-attribute 'mode-line-inactive   nil :foreground fg-in :background bg-in :box nil)
  ;; header line
  (set-face-attribute 'header-line          nil :foreground fg-in :background bg-in :box nil))

(defvar mirage/dark-modeline-colors '("#cfcfcf" "#cfcfcf" "#454545" "#242424")
  "Dark modeline HTML colors: FOREGROUND, FOREGROUND-INACTIVE, BACKGROUND, BACKGROUND-INACTIVE")

(defvar mirage/light-modeline-colors '("#616161" "#878787" "#c4c4c4" "#ededed")
  "Light modeline HTML colors: FOREGROUND, FOREGROUND-INACTIVE, BACKGROUND, BACKGROUND-INACTIVE")

(defun mirage/set-dark-modeline-colors ()
  (apply 'mirage/modeline-set-colors mirage/dark-modeline-colors))

(defun mirage/set-light-modeline-colors ()
  (apply 'mirage/modeline-set-colors mirage/light-modeline-colors))

(add-to-list 'mirage/theme-advice-dark  #'mirage/set-dark-modeline-colors)
(add-to-list 'mirage/theme-advice-light #'mirage/set-light-modeline-colors)

;; reload active theme
(let ((active-theme (car custom-enabled-themes)))
  (if active-theme (enable-theme active-theme)))

(provide 'mirage-extension-theme-modeline)
;;; mirage-theme-modeline.el ends here
