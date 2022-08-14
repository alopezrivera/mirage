(setq calendar-latitude      52.00667)
(setq calendar-longitude     4.355561)
(setq calendar-loadtion-name "Delft")
(setq calendar-standard-time-zone-name "CEST")
(setq calendar-daylight-time-zone-name "CET")

(straight-use-package 'circadian)
(setq circadian-themes `((:sunrise . ,light)  
			      (:sunset  . ,dark)))
(circadian-setup)

(provide 'shapes-modules-circadian)
;;; shapes-circadian.el ends here
