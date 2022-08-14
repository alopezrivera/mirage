(straight-use-package 'treemacs)
(require 'treemacs)

(defvar custom/treemacs-ignored '(".*__pycache__.*")
  "Files and directories ignored by treemacs")

(defun custom/treemacs-ignore-filter (file _)
  (cl-loop for ignored in custom/treemacs-ignored
	   if (string-match ignored file)
	      return t
	   finally return nil))
(push #'custom/treemacs-ignore-filter treemacs-ignored-file-predicates)

(cl-loop for binding in '(("M-0"       . treemacs-select-window)
			      ("C-x t 1"   . treemacs-delete-other-windows)
			      ("C-x t t"   . treemacs)
			      ("C-x t d"   . treemacs-select-directory)
			      ("C-x t B"   . treemacs-bookmark)
			      ("C-x t C-t" . treemacs-find-file)
			      ("C-x t M-t" . treemacs-find-tag))
	 collect (global-set-key (kbd (car binding)) (cdr binding)))

(setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
      treemacs-deferred-git-apply-delay        0.5
      treemacs-directory-name-transformer      #'identity
      treemacs-display-in-side-window          t
      treemacs-eldoc-display                   'simple
      treemacs-file-event-delay                5000
      treemacs-file-extension-regex            treemacs-last-period-regex-value
      treemacs-file-follow-delay               0.2
      treemacs-file-name-transformer           #'identity
      treemacs-follow-after-init               t
      treemacs-expand-after-init               t
      treemacs-find-workspace-method           'find-for-file-or-pick-first
      treemacs-git-command-pipe                ""
      treemacs-goto-tag-strategy               'refetch-index
      treemacs-indentation                     2
      treemacs-indentation-string              " "
      treemacs-is-never-other-window           nil
      treemacs-max-git-entries                 5000
      treemacs-missing-project-action          'ask
      treemacs-move-forward-on-expand          nil
      treemacs-no-png-images                   nil
      treemacs-no-delete-other-windows         t
      treemacs-project-follow-cleanup          nil
      treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
      treemacs-position                        'left
      treemacs-read-string-input               'from-child-frame
      treemacs-recenter-distance               0.1
      treemacs-recenter-after-file-follow      nil
      treemacs-recenter-after-tag-follow       nil
      treemacs-recenter-after-project-jump     'always
      treemacs-recenter-after-project-expand   'on-distance
      treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
      treemacs-show-cursor                     nil
      treemacs-show-hidden-files               t
      treemacs-silent-filewatch                nil
      treemacs-silent-refresh                  nil
      treemacs-sorting                         'alphabetic-asc
      treemacs-select-when-already-in-treemacs 'move-back
      treemacs-space-between-root-nodes        t
      treemacs-tag-follow-cleanup              t
      treemacs-tag-follow-delay                1.5
      treemacs-text-scale                      nil
      treemacs-user-mode-line-format           nil
      treemacs-user-header-line-format         nil
      treemacs-wide-toggle-width               70
      treemacs-width                           35
      treemacs-width-increment                 1
      treemacs-width-is-initially-locked       t
      treemacs-workspace-switch-cleanup        nil)

;; The default width and height of the icons is 22 pixels. If you are
;; using a Hi-DPI display, uncomment this to double the icon size.
;; (treemacs-resize-icons 44)

(treemacs-follow-mode t)
(treemacs-filewatch-mode t)
(treemacs-fringe-indicator-mode 'always)

(pcase (cons (not (null (executable-find "git")))
             (not (null treemacs-python-executable)))
  (`(t . t)
   (treemacs-git-mode 'deferred))
  (`(t . _)
   (treemacs-git-mode 'simple)))

(treemacs-hide-gitignored-files-mode nil)

(straight-use-package 'treemacs-icons-dired)

(straight-use-package 'treemacs-projectile)

(straight-use-package 'treemacs-tab-bar)

(straight-use-package 'treemacs-magit)

(provide 'shapes-modules-treemacs)
;;; shapes-treemacs.el ends here
