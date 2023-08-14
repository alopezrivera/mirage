(defcustom light-theme 'nil
  "Light theme")
(defcustom dark-theme  'nil
  "Dark theme")

;; creation
(shapes-module "autothemer")

;; theme switching
(shapes-extend "theme-switch")

;; scheduling
(shapes-module "circadian")

(provide 'shapes-layer-themes)
;;; shapes-themes.el ends here
