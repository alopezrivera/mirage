(require 'org-paragraph (concat user-emacs-directory "elisp/packages/org-paragraph.el"))

(defun mirage/org-meta-arrows-h (orig-fun &rest args)
  "Paragraph indentation with `org-meta<arrows>'.
Furthermore, if a region is active and its
beginning lies on an Org Mode heading,
`mirage/org-command-expand-region' to execute ORIG-FUN."
  (interactive)
  (cond ((mirage/org-relative-line-paragraph) (mirage/org-paragraph orig-fun args))
	((region-active-p)                    (mirage/org-indent-region orig-fun args))
	(t                                    (apply orig-fun args))))

(advice-add 'org-metaleft  :around #'mirage/org-meta-arrows-h)
(advice-add 'org-metaright :around #'mirage/org-meta-arrows-h)

(defun mirage/org-meta-arrows-v (orig-fun &rest args)
  (interactive)
  (if (mirage/org-at-ellipsis)
      (progn (beginning-of-visual-line) (end-of-line)))
  (apply orig-fun args)
  (if (mirage/org-relative-line-heading-folded)
      (outline-hide-subtree)))

(advice-add 'org-metaup   :around #'mirage/org-meta-arrows-v)
(advice-add 'org-metadown :around #'mirage/org-meta-arrows-v)

(provide 'mirage-module-org-paragraph)
;;; mirage-org-paragraph.el ends here
