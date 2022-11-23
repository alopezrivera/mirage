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

(defun custom/org-fix-bleed-end-line-block (from to flag spec)
  "Toggle fontification of last char of block end lines when cycling.

This avoids the bleeding of `org-block-end-line' when block is
folded."
  (when (and (eq spec 'org-hide-block)
             (/= (point-max) to))
    (save-excursion
      (if flag
          (font-lock-unfontify-region to (1+ to))
        (font-lock-flush to (1+ to))))))

(advice-add 'org-flag-region :after #'custom/org-fix-bleed-end-line-block)

(defun custom/org-fix-bleed-end-line-cycle (state)
  "Toggle fontification of last char of block lines when cycling.

This avoids the bleeding of `org-block-end-line' when outline is
folded."
  (save-excursion
    (when org-fontify-whole-block-delimiter-line
      (let ((case-fold-search t)
            beg end)
        (cond ((memq state '(overview contents all))
               (setq beg (point-min)
                     end (point-max)))
              ((memq state '(children folded subtree))
               (setq beg (point)
                     end (org-end-of-subtree t t))))
        (when beg           ; should always be true, but haven't tested enough
          (goto-char beg)
          (while (search-forward "#+end" end t)
            (end-of-line)
            (unless (= (point) (point-max))
              (if (org-invisible-p (1- (point)))
                  (font-lock-unfontify-region (point) (1+ (point)))
                (font-lock-flush (point) (1+ (point)))))))))))

(add-hook 'org-cycle-hook #'custom/org-fix-bleed-end-line-cycle)

;; Set indentation of code blocks to 0
(setq org-edit-src-content-indentation 0)

;; Indent code blocks appropriately when inside headers
(setq org-src-preserve-indentation     nil)

;; Make code indentation reasonable
(setq org-src-tab-acts-natively        t)

;; Language packages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python     . t)
   (shell      . t)
   (latex      . t)))

(defun org-babel-bash-initiate-session (&optional session _params)
  "Initiate a bash/sh session named SESSION according to PARAMS."
  (org-babel-sh-initiate-session session _params))

(setq org-babel-python-command "python3")

;; suppress security confirmation when evaluating code
(setq org-confirm-babel-evaluate nil)

(provide 'shapes-module-org-babel)
;;; shapes-org-babel.el ends here
