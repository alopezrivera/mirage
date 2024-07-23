(mirage-module 'moody)

;; modeline height
(setq moody-mode-line-height 22)

;; adjust modeline elements to enable moody
(let ((line (face-attribute 'mode-line :underline)))
  (set-face-attribute 'mode-line          nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :underline  line)
  (set-face-attribute 'mode-line          nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :box        nil))

;; extensions
(mirage-extend 'theme-modeline)

(provide 'mirage-layer-modeline-moody)
;;; mirage-modeline-moody.el ends here
