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
             ;; TODO: Fix this case. `hs-show-block' needs to be
             ;; called twice to open all folds of the parent
             ;; block.
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

(define-key hs-minor-mode-map (kbd "C-<return>") #'custom/hs-cycle)

;; lsp-mode
(use-package lsp-mode)

;; lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

;; Hook
(add-hook 'python-mode-hook #'lsp-mode)

(lsp-register-custom-settings
 '(("pyls.plugins.pyls_mypy.enabled"   t t)
   ("pyls.plugins.pyls_mypy.live_mode" nil t)
   ("pyls.plugins.pyls_black.enabled"  t t)
   ("pyls.plugins.pyls_isort.enabled"  t t)))

(use-package pyvenv)

;; Default venv
(setq pyvenv-workon "emacs")

;; 
(pyvenv-tracking-mode 1)

;; flycheck

;; company-mode

(straight-use-package 'projectile)

;; helm projectile

(provide 'ide)
