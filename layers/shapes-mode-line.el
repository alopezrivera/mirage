(defcustom modeline 'doom-modeline-mode
  "Mode line")

;; mode lines
(shapes-module "nano-modeline")
(shapes-module "doom-modeline")
(shapes-module "spaceline")

;; mode line initialization hook
(add-hook 'after-init-hook modeline)

(provide 'shapes-layer-mode-line)
;;; shapes-mode-line.el ends here
