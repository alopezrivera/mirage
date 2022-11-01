(flet ((repo (name) (concat config-directory "straight/repos/" name)))
  ;; install and rename repository directory to "catppuccin-themes"
  (if (not (file-exists-p (repo "catppuccin-themes/catppuccin-theme.el")))
      (progn (straight-use-package '(catppuccin-themes :type git :host github :repo "catppuccin/emacs"))
             (copy-directory (repo "emacs")
                             (repo "catppuccin-themes"))
             (delete-directory (repo "emacs") t)))
  (require 'catppuccin-theme           (repo "catppuccin-themes/catppuccin-theme.el"))
  (require 'catppuccin-frappe-theme    (repo "catppuccin-themes/catppuccin-frappe-theme.el"))
  (require 'catppuccin-latte-theme     (repo "catppuccin-themes/catppuccin-latte-theme.el"))
  (require 'catppuccin-mocha-theme     (repo "catppuccin-themes/catppuccin-mocha-theme.el"))
  (require 'catppuccin-macchiato-theme (repo "catppuccin-themes/catppuccin-macchiato-theme.el")))

(provide 'shapes-module-catppuccin-themes)
;;; shapes-catppuccin-themes.el ends here
