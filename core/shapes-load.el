(defun shapes-load (type component)
  "Load a shapeshifter COMPONENT of the given TYPE"
  (require (intern (concat "shapes-" type "-" component)) (concat config-directory type "s" "/shapes-" component ".el")))

;; shapeshifter modules
(defun shapes-module (module)
  "Load a shapeshifter MODULE by name"
  (shapes-load "module" module))

;; shapeshifter layers
(defun shapes-layer (layer)
  "Load a shapeshifter LAYER by name"
  (shapes-load "layer" layer))

;; shapeshifter extensions
(defun shapes-extend (extension)
  "Load a shapeshifter EXTENSION by name"
  (shapes-load "extension" extension))

(provide 'shapes-core-load)
;;; shapes-load.el ends here
