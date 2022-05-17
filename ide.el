(require 'hideshow)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(defun custom/hs-cycle (&optional level)
  (interactive "p")
  (save-excursion
    (let (message-log-max (inhibit-message t))
      (if (= level 1)
	  (pcase last-command
	    ('hs-cycle
	     (hs-hide-level 1)
	   (setq this-command 'hs-cycle-children))
	    ('hs-cycle-children
	     TODO: Fix this case. `hs-show-block' needs to be
	     called twice to open all folds of the parent
	     block.
	     (save-excursion (hs-show-block))
	     (hs-show-block)
	     (setq this-command 'hs-cycle-subtree))
	    ('hs-cycle-subtree
	     (hs-hide-block))
	    (_
	     (if (not (hs-already-hidden-p))
		 (hs-hide-block)
	       (hs-hide-level 1)
	       (setq this-command 'hs-cycle-children))))
	(hs-hide-level level)
	(setq this-command 'hs-hide-level)))))

(defun custom/hs-global-cycle ()
  (interactive)
  (pcase last-command
    ('hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'hs-global-show))
    (_ (hs-hide-all))))

(define-key hs-minor-mode-map (kbd "C-\\") #'custom/hs-cycle)

;; company
(straight-use-package 'company)

;; flycheck
(straight-use-package 'flycheck)
(add-hook 'prog-mode-hook #'flycheck-mode)

;; elpy
(straight-use-package 'elpy)
(elpy-enable)

(setq elpy-rpc-timeout 5)

(setq elpy-rpc-backend "jedi")

(setq elpy-rpc-python-command "python3")

(define-key elpy-mode-map (kbd "C-M-n") 'elpy-nav-forward-block)
(define-key elpy-mode-map (kbd "C-M-p") 'elpy-nav-backward-block)

;; pyenv
(straight-use-package 'pyenv)

;; projectile
(straight-use-package 'projectile)

(provide 'ide)
