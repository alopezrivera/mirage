(setq light 'modus-operandi)

(setq dark  'sweet)

(use-package modus-themes)
(modus-themes-load-themes)

(straight-use-package 'sweet-theme)
(require 'sweet-theme)

;; Bar
(setq-default doom-modeline-bar-width 0.01)

;; Color
(defun custom/modeline-color (bg bg-in face face-in)
  "Set the color of the mode line and blend the 
`doom-modeline-bar' with the background."
  (set-face-attribute 'mode-line          nil :foreground face    :background bg    :box nil)
  (set-face-attribute 'mode-line-inactive nil :foreground face-in :background bg-in :box nil))

(defun custom/dark-modeline ()
  "Mode line for light themes."
  (custom/modeline-color "#3d3d3d" "#000000" "#cfcfcf" "#cfcfcf"))

(defun custom/light-modeline ()
  "Mode line for dark themes."
  (custom/modeline-color "#fff0ff" "#ededed" "#616161" "#878787"))

(defun custom/dark-line-numbers ()
  "Line numbers for light themes."
  (set-face-attribute 'line-number nil :foreground "#cfcfcf" :background "#262626"))

(defun custom/light-line-numbers ()
  "Line numbers for dark themes."
  (set-face-attribute 'line-number nil :foreground "#878787" :background "#ededed"))

(defun custom/operandi-advice ()
  (custom/light-modeline)
  (custom/light-line-numbers))

(defun custom/vivendi-advice ()
  (custom/dark-modeline)
  (custom/dark-line-numbers))

(defun custom/theme-specific-advice (_orig-fun &rest args)
  "Apply theme-specific advice when enabling themes, and
preserve modeline status through theme changes."
  (setq modeline-status mode-line-format)
  (apply _orig-fun args)
  (cond ((string-equal (nth 0 args) "modus-operandi") (custom/operandi-advice))
	((string-equal (nth 0 args) "modus-vivendi")  (custom/vivendi-advice)))
  (setq mode-line-format modeline-status))

;; enable-theme
(dolist (load-fn '(enable-theme
		   circadian-enable-theme))
  (advice-add load-fn :around #'custom/theme-specific-advice))

(defun custom/theme-toggle ()
  "Toggle between `dark' and `light' themes
using `enable-theme'"
  (interactive)
  (let ((theme (nth 0 custom-enabled-themes)))
    (cond ((string-equal theme light) (progn (disable-theme light)
							  (enable-theme  dark)))
	  (t                                       (progn (disable-theme dark)
							  (enable-theme  light))))))

(global-set-key (kbd "C-t") 'custom/theme-toggle)

(setq calendar-latitude      52.00667)
(setq calendar-longitude     4.355561)
(setq calendar-loadtion-name "Delft")
(setq calendar-standard-time-zone-name "CEST")
(setq calendar-daylight-time-zone-name "CET")

(use-package circadian
  :config
  (setq circadian-themes `((:sunrise . ,light)  
			   (:sunset  . ,dark)))
  (circadian-setup))

;; Provide theme
(provide 'theme)
