(defcustom light 'modus-operandi
  "Light theme")
(defcustom dark  'modus-vivendi
  "Dark theme")

;; theme bundles
(shapes-module "ef-themes")
(shapes-module "nano-theme")
(shapes-module "doom-themes")
(shapes-module "modus-themes")
(shapes-module "solarized-theme")
(shapes-module "catppuccin-themes")
;; standalone themes
(shapes-module "sweet-theme")
(shapes-module "graphite-theme")
(shapes-module "chocolate-theme")

;; extensions
(shapes-extend "theme-switch")

;; scheduling
(shapes-module "circadian")

(provide 'shapes-layer-themes)
;;; shapes-themes.el ends here
