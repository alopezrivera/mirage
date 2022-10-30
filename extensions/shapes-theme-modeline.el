;;; -*- lexical-binding: t; -*-

(defun custom/modeline-set-colors (fg fg-in bg bg-in)
  "Set the color of the mode and header lines and blend the 
`doom-modeline-bar' with the background."
  (set-face-attribute 'mode-line            nil :foreground fg    :background bg    :box nil)
  (set-face-attribute 'mode-line-inactive   nil :foreground fg-in :background bg-in :box nil)
  ;; header line
  (set-face-attribute 'header-line          nil :foreground fg-in :background bg-in :box nil))

(defvar custom/dark-modeline-colors '("#cfcfcf" "#cfcfcf" "#454545" "#242424")
  "Dark modeline HTML colors: FOREGROUND, FOREGROUND-INACTIVE, BACKGROUND, BACKGROUND-INACTIVE")

(defvar custom/light-modeline-colors '("#616161" "#878787" "#c4c4c4" "#ededed")
  "Light modeline HTML colors: FOREGROUND, FOREGROUND-INACTIVE, BACKGROUND, BACKGROUND-INACTIVE")

(defun custom/set-dark-modeline-colors ()
  (apply 'custom/modeline-set-colors custom/dark-modeline-colors))

(defun custom/set-light-modeline-colors ()
  (apply 'custom/modeline-set-colors custom/light-modeline-colors))

(add-to-list 'custom/theme-advice-dark  #'custom/set-dark-modeline-colors)
(add-to-list 'custom/theme-advice-light #'custom/set-light-modeline-colors)

;; reload active theme
(let ((active-theme (car custom-enabled-themes)))
  (if active-theme (enable-theme active-theme)))

(provide 'shapes-extension-theme-modeline)
;;; shapes-theme-modeline.el ends here
