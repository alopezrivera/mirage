;; accent typefaces
(defvar custom/accents '(custom/italic))

(defun custom/theme-accents (orig-fun &rest args)
  "Many themes will override certain face *attributes*, such as `italic'. To prevent
this, this function loops over all accent typefaces in `custom/accents', which contains
faces (defined with `defface') named ~custom/<attribute>~, and makes the ~<attribute>~
inherit from ~custom/<attribute>~.

As such, when this function is run, the `italic' face attribute will be made to
inherit from `custom/italic' as in the expression below.

   (set-face-attribute 'italic nil :inherit 'custom/italic)

Thus, our preferred accent typefaces will stand whatever harassment they may be put
through as a theme loads."
  ;; load theme
  (apply orig-fun args)
  ;; restore accents
  (cl-loop for accent in custom/accents
	   collect (let ((face (intern (car (last (split-string (symbol-name accent) "/"))))))
		     (set-face-attribute face nil :inherit accent))))

(advice-add 'load-theme :around #'custom/theme-accents)
