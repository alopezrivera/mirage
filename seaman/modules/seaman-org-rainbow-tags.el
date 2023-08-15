(straight-use-package '(org-rainbow-tags :type git :host github :repo "KaratasFurkan/org-rainbow-tags"))
(require 'org-rainbow-tags)

(add-hook 'org-mode-hook #'org-rainbow-tags-mode)

(setq org-rainbow-tags-hash-start-index 5)

(provide 'seaman-module-org-rainbow-tags)
;;; seaman-org-rainbow-tags.el ends here
