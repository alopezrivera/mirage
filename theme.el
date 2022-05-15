(use-package modus-themes)

(modus-themes-load-themes)

(defun custom/operandi-advice ()
  (custom/light-modeline)
  (custom/light-line-numbers))

(defun custom/vivendi-advice ()
  (custom/dark-modeline)
  (custom/dark-line-numbers))

(defun custom/theme-specific-advice (_orig-fun &rest args)
  (setq modeline-status mode-line-format)
  (apply _orig-fun args)
  (cond ((string-equal (nth 0 args) "modus-operandi") (custom/operandi-advice))
	      ((string-equal (nth 0 args) "modus-vivendi")  (custom/vivendi-advice)))
  (setq mode-line-format modeline-status))

(advice-add 'enable-theme :around #'custom/theme-specific-advice)

(setq calendar-latitude      52.00667)
(setq calendar-longitude     4.355561)
(setq calendar-loadtion-name "Delft")
(setq calendar-standard-time-zone-name "CEST")
(setq calendar-daylight-time-zone-name "CET")

(use-package circadian
  :config
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(defun custom/modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes.
This uses `enable-theme' instead of the standard method of
`load-theme'.  The technicalities are covered in the Modus themes
manual."
  (interactive)
  (pcase (modus-themes--current-theme)
    ('modus-operandi (progn (enable-theme 'modus-vivendi)
                            (disable-theme 'modus-operandi)))
    ('modus-vivendi (progn (enable-theme 'modus-operandi)
                            (disable-theme 'modus-vivendi)))
    (_ (error "No Modus theme is loaded; evaluate `modus-themes-load-themes' first"))))

(global-set-key (kbd "C-t") 'custom/modus-themes-toggle)

;; Provide theme
(provide 'theme)
