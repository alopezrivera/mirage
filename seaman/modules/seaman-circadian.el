(setq calendar-latitude      52.00667)
(setq calendar-longitude     4.355561)
(setq calendar-loadtion-name "Delft")
(setq calendar-standard-time-zone-name "CEST")
(setq calendar-daylight-time-zone-name "CET")

(straight-use-package 'circadian)

(add-hook 'after-init-hook (lambda () (progn (setq circadian-themes `((:sunrise . ,light-theme)  
			                                              (:sunset  . ,dark-theme)))
                                             (circadian-setup))))

(provide 'mirage-module-circadian)
;;; mirage-circadian.el ends here
