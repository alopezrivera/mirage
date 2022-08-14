(defcustom light 'modus-operandi
  "Light theme")
(defcustom dark  'modus-vivendi
  "Dark theme")

;; themes
(shapes-module "nano-theme")
(shapes-module "doom-themes")
(shapes-module "modus-themes")
(shapes-module "sweet-theme")
(shapes-module "graphite-theme")
(shapes-module "chocolate-theme")

;; extensions
(shapes-extend "themes")

;; scheduling
(shapes-module "circadian")

(provide 'shapes-layers-themes)
;;; shapes-themes.el ends here
