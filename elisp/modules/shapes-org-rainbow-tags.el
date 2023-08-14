(straight-use-package '(org-rainbow-tags :type git :host github :repo "KaratasFurkan/org-rainbow-tags"))
(require 'org-rainbow-tags)

(add-hook 'org-mode-hook #'org-rainbow-tags-mode)

(setq org-rainbow-tags-hash-start-index 5)

(provide 'shapes-module-org-rainbow-tags)
;;; shapes-org-rainbow-tags.el ends here
