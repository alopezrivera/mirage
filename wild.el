;;; -*- lexical-binding: t; -*-

(setq config-directory "~/.emacs.d/")
(setq startup-buffers '("~/.emacs.d/wild.el"))

(global-set-key (kbd "C-M-p") (lambda () (interactive) (insert
"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")))

;; display
(setq-default frame-title-format '("Emacs [%m] %b"))
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; startup buffers
(dolist (startup-b startup-buffers)
  (find-file-noselect startup-b))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; modus
(straight-use-package 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-operandi)

;; selectrum
(straight-use-package 'selectrum)
(selectrum-mode +1)

;; magit
(straight-use-package 'magit)

;; elpy
(straight-use-package 'elpy)
(add-hook 'python-mode-hook #'elpy-enable)

;; flycheck
(straight-use-package 'flycheck)
(add-hook 'prog-mode-hook #'flycheck-mode)

;; hideshow
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

(define-key hs-minor-mode-map (kbd "C-\\") #'custom/hs-cycle)

;; declare
(provide 'wild)
