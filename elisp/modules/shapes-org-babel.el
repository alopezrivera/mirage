;; src edit buffer in current window
(setq org-src-window-setup "current-window")

;; Language packages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python     . t)
   (C          . t)
   (shell      . t)
   (latex      . t)))

(defun org-babel-bash-initiate-session (&optional session _params)
  "Initiate a bash/sh session named SESSION according to PARAMS."
  (org-babel-sh-initiate-session session _params))

(setq org-babel-python-command "python3")

;; suppress security confirmation when evaluating code
(setq org-confirm-babel-evaluate nil)

;; Set indentation of code blocks to 0
(setq org-edit-src-content-indentation 0)

;; Indent code blocks appropriately when inside headers
(setq org-src-preserve-indentation     nil)

;; Make code indentation reasonable
(setq org-src-tab-acts-natively        t)

(setq org-babel-default-header-args
      '((:noweb   . "yes")
        (:async   . "yes")
        (:session . "ob-session")
        (:results . "replace output")
        (:exports . "code")
        (:cache   . "no")
        (:hlines  . "no")
        (:tangle  . "no")))

(provide 'shapes-module-org-babel)
;;; shapes-org-babel.el ends here
