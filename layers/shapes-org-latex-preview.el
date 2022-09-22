(shapes-module "org-fragtog")

(setq org-format-latex-options
        (list :foreground 'default
              :scale      1.35))

(setq custom/org-latex-preview-class-args "fleqn")
(setq custom/org-latex-preview-width      "16.5cm")

(setq org-format-latex-header
      (string-join `("\\documentclass[" ,custom/org-latex-preview-class-args "]{article}"
		         "\\usepackage[usenames]{color}"
			
			 "\\usepackage{bm}"
			
			 "\\pagestyle{empty}"
			 "\\setlength{\\textwidth}{" ,custom/org-latex-preview-width "}"
			 "\\addtolength{\\textwidth}{-3cm}"
			 "\\setlength{\\oddsidemargin}{1.5cm}"
			 "\\addtolength{\\oddsidemargin}{-2.54cm}"
			 "\\setlength{\\evensidemargin}{\\oddsidemargin}"
			 "\\setlength{\\textheight}{\\paperheight}"
			 "\\addtolength{\\textheight}{-\\headheight}"
			 "\\addtolength{\\textheight}{-\\headsep}"
			 "\\addtolength{\\textheight}{-\\footskip}"
			 "\\addtolength{\\textheight}{-3cm}"
			 "\\setlength{\\topmargin}{1.5cm}"
			 "\\addtolength{\\topmargin}{-2.54cm}")
		   "\n"))

;; SVG LaTeX equation preview
(setq org-latex-create-formula-image-program 'dvisvgm)

;; theme-specific LaTeX preview directory
(defun custom/latex-preview-directory ()
  "Set `org-preview-latex-image-directory' to the SVG
LaTeX preview directory of the current theme"
  (setq org-preview-latex-image-directory
   (concat "/tmp/ltximg/" (custom/get-active-theme) "/")))

(defun custom/latex-preview-reload ()
  "Reload all LaTeX previews in buffer,
ensuring the LaTeX preview directory
matches the current theme."
  (if (string-equal major-mode "org-mode")
      (progn (org-latex-preview '(64))
	        (custom/latex-preview-directory)
		(org-latex-preview '(16)))))

(add-hook 'org-mode-hook #'custom/latex-preview-reload)

(provide 'shapes-layer-org-latex-preview)
;;; shapes-org-latex-preview.el ends here
