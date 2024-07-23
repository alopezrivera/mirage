(defcustom light-theme 'nil
  "Light theme")
(defcustom dark-theme  'nil
  "Dark theme")

;; creation
(mirage-module 'autothemer)

;; theme switching
(mirage-extend 'theme-switch)

;; scheduling
(mirage-module 'circadian)

(provide 'mirage-layer-themes)
;;; mirage-themes.el ends here
