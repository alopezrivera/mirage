(require 'org-paragraph (concat config-directory "packages/org-paragraph.el"))

(defun custom/org-meta-arrows-h (orig-fun &rest args)
  "Paragraph indentation with `org-meta<arrows>'.
Furthermore, if a region is active and its
beginning lies on an Org Mode heading,
`custom/org-command-expand-region' to execute ORIG-FUN."
  (interactive)
  (cond ((custom/org-relative-line-paragraph) (custom/org-paragraph orig-fun args))
	    ((region-active-p)                    (custom/org-indent-region orig-fun args))
	    (t                                    (apply orig-fun args))))

(advice-add 'org-metaleft  :around #'custom/org-meta-arrows-h)
(advice-add 'org-metaright :around #'custom/org-meta-arrows-h)

(defun custom/org-meta-arrows-v (orig-fun &rest args)
  (interactive)
  (if (custom/org-at-ellipsis)
      (progn (beginning-of-visual-line) (end-of-line)))
  (apply orig-fun args)
  (if (custom/org-relative-line-heading-folded)
      (outline-hide-subtree)))

(advice-add 'org-metaup   :around #'custom/org-meta-arrows-v)
(advice-add 'org-metadown :around #'custom/org-meta-arrows-v)

(provide 'shapes-modules-org-paragraph)
;;; shapes-org-paragraph.el ends here
