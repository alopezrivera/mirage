(straight-use-package 'org-download)

(dolist (mode '(org-mode-hook
		     dired-mode-hook))
  (add-hook mode 'org-download-enable))
