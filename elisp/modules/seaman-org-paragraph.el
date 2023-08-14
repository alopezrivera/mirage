(require 'org-paragraph (concat user-emacs-directory "elisp/packages/org-paragraph.el"))

(defun seaman/org-meta-arrows-h (orig-fun &rest args)
  "Paragraph indentation with `org-meta<arrows>'.
Furthermore, if a region is active and its
beginning lies on an Org Mode heading,
`seaman/org-command-expand-region' to execute ORIG-FUN."
  (interactive)
  (cond ((seaman/org-relative-line-paragraph) (seaman/org-paragraph orig-fun args))
	((region-active-p)                    (seaman/org-indent-region orig-fun args))
	(t                                    (apply orig-fun args))))

(advice-add 'org-metaleft  :around #'seaman/org-meta-arrows-h)
(advice-add 'org-metaright :around #'seaman/org-meta-arrows-h)

(defun seaman/org-meta-arrows-v (orig-fun &rest args)
  (interactive)
  (if (seaman/org-at-ellipsis)
      (progn (beginning-of-visual-line) (end-of-line)))
  (apply orig-fun args)
  (if (seaman/org-relative-line-heading-folded)
      (outline-hide-subtree)))

(advice-add 'org-metaup   :around #'seaman/org-meta-arrows-v)
(advice-add 'org-metadown :around #'seaman/org-meta-arrows-v)

(provide 'seaman-module-org-paragraph)
;;; seaman-org-paragraph.el ends here
