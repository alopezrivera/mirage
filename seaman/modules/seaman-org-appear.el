(setq org-hide-emphasis-markers t)

(setq org-hidden-keywords '(title))

;; org-appear
(straight-use-package '(org-appear :type git :host github :repo "awth13/org-appear"))
(add-hook 'org-mode-hook 'org-appear-mode)

;; links
(setq org-appear-autolinks t)

;; keywords
(setq org-appear-autokeywords t)

;; symbols
(setq org-appear-autoentities t)

;; subscripts and superscripts
(setq org-appear-autosubmarkers t)
(setq org-appear-inside-latex t)

(provide 'seaman-module-org-appear)
;;; seaman-org-appear.el ends here
