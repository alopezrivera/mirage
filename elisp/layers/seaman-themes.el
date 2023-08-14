(defcustom light-theme 'nil
  "Light theme")
(defcustom dark-theme  'nil
  "Dark theme")

;; creation
(seaman-module 'autothemer)

;; theme switching
(seaman-extend 'theme-switch)

;; scheduling
(seaman-module 'circadian)

(provide 'seaman-layer-themes)
;;; seaman-themes.el ends here
