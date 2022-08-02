;; org-fragtog
(straight-use-package 'org-fragtog)
(require 'org-fragtog)

(add-hook 'org-mode-hook #'org-fragtog-mode)
