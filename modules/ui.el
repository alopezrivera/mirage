;; default face
(set-face-attribute 'default nil        :font "Fira Code Retina" :height 93)

;; fixed pitch face
(set-face-attribute 'fixed-pitch nil    :font "Fira Code Retina" :height 93)

;; variable pitch face
(set-face-attribute 'variable-pitch nil :font "PT Sans"  :height 105 :weight 'regular)

;; italic
(defface custom/italic
  '((t :font "Victor Mono" :height  86 :weight  bold :slant italic))
  "Italic typeface")

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

;; Mode line
(set-face-attribute 'mode-line nil :height 85 :inherit 'fixed-pitch)

;; Symbol library
(straight-use-package 'all-the-icons)

(straight-use-package 'svg-tag-mode)

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                    :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag value nil
                           :stroke 0 :margin 0)) :ascent 'center)))

(setq svg-tag-tags
      `(
        ;; Org tags
        (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
        
        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority 
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))
        
        ;; TODO / DONE
        ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


        ;; Citation of the form [cite:@Knuth:1984]
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))
        
        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (with or without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

;; Highlight HTML color strings in their own color
(straight-use-package 'rainbow-mode)

(display-time-mode t)

;; Customize names displayed in mode line
(straight-use-package 'delight)
(require 'delight)

;; Remove default modes from mode line
(delight '((global-command-log-mode nil "")
	      (olivetti-mode           nil "")
	      (which-key-mode          nil "")
	      (visual-line-mode        nil "simple")
	      (buffer-face-mode        nil "simple")
	      (org-indent-mode         nil "org")
	      (eldoc-mode              nil "eldoc")
	      ;; Major modes
	      (emacs-lisp-mode "EL" :major)))

;; Provide theme
(provide 'ui)
