;; `org-in-src-block-p' gives false positives as of Org Mode 9.5.3. For
;; this reason, determine if cursor in src block with the more reliable
;; `org-babel-where-is-src-block-head'
(defun seaman/org-in-src-block-p (orig-fun &rest args)
  (if args
      (apply orig-fun args)
    (org-babel-where-is-src-block-head)))

(advice-add 'org-in-src-block-p :around #'seaman/org-in-src-block-p)

(defun seaman/org-at-ellipsis (&optional position)
  (or (seaman/org-at-ellipsis-h position) (seaman/org-at-ellipsis-l position)))

(defun seaman/org-at-ellipsis-l (&optional position)
  (and (seaman/org-relative-line-list-folded) (seaman/at-point 'end-of-visual-line)))

(defun seaman/org-at-ellipsis-h (&optional position) 
  (and (seaman/org-relative-line-heading-folded) (seaman/at-point 'end-of-visual-line)))

(defun seaman/org-at-keyword (&optional number)
  (seaman/relative-line-regex "^#+.*$" number))

(defun seaman/org-at-heading (&optional point)
  (let ((point (or point (point))))
    (save-excursion (goto-char point) (seaman/org-relative-line-heading))))

(defun seaman/org-at-bol-list () 
  (and (seaman/org-relative-line-list) (seaman/at-point 'beginning-of-line-text)))

(defun seaman/org-at-bol-heading () 
  (and (seaman/org-relative-line-heading) (seaman/at-point 'seaman/org-goto-heading-bol)))

(defun seaman/org-at-eol-heading ()
  (and (seaman/org-relative-line-heading) (eolp) (not (seaman/org-at-ellipsis-h)) (not (seaman/org-relative-line-heading-empty))))

(defun seaman/org-after-list-or-indent ()
  (or (seaman/org-relative-line-list -1) (seaman/relative-line-indented -1)))

(defun seaman/org-relative-line-list (&optional number)
  (seaman/relative-line (lambda () (progn (beginning-of-line-text) (org-at-item-p)))  number))

(defun seaman/org-relative-line-heading (&optional number)
  (seaman/relative-line 'org-at-heading-p number))

(defun seaman/org-relative-line-paragraph (&optional number)
  "Determine whether the current line -or the NUMBER'th line relative to it
is an indented paragraph."
  (let ((number (or number 0)))
    (and (not (seaman/org-relative-line-heading number))
	       (not (seaman/org-relative-line-list    number))
	       (not (org-in-src-block-p))
	       (seaman/relative-line-indented number)
	       (or  (seaman/org-relative-line-list      (- number 1))
		    (seaman/org-relative-line-paragraph (- number 1))))))

(defun seaman/org-relative-line-list-empty (&optional number)
  (and (seaman/org-relative-line-list)
       (or (seaman/relative-line-regex "^[[:blank:]]*[-+*]\\{1\\}[[:blank:]]+$" number)
	         (seaman/relative-line-regex "^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]+$" number))))

(defun seaman/org-relative-line-list-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded list item."
  (seaman/relative-line (lambda () (and (org-at-item-p) (invisible-p (point-at-eol)))) number))

(defun seaman/org-relative-line-heading-empty (&optional number)
  (seaman/relative-line (lambda () (beginning-of-line-text) (org-point-at-end-of-empty-headline)) number))

(defun seaman/org-relative-line-heading-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded heading."
  (seaman/relative-line (lambda () (and (org-at-heading-p) (invisible-p (point-at-eol)))) number))

(defun seaman/org-relative-line-heading-or-list (&optional number)
  (seaman/relative-line 'org-at-heading-or-item-p number))

(defun seaman/org-subtree-blank ()
  "Return t if the current subtree consists of
a `seaman/region-blank'."
  (interactive)
  (apply 'seaman/region-blank (seaman/org-get-subtree-region)))

(defun seaman/org-subtree-empty ()
  (interactive)
  (string-equal "" (seaman/org-get-subtree-content)))

(defun seaman/org-headings-follow ()
  (let ((pos (seaman/get-point 'beginning-of-visual-line)))
    (save-excursion (seaman/org-goto-heading-next)
		           (and (not (= pos (point))) (seaman/org-relative-line-heading)))))

(defun seaman/org-headings-precede ()
  (let ((pos (seaman/get-point 'beginning-of-visual-line)))
    (save-excursion (seaman/org-goto-heading-previous)
		          (and (not (= pos (point))) (seaman/org-relative-line-heading)))))

(defun seaman/org-subtree-blank-up-to-point ()
  (interactive)
  (let ((heading-eol (save-excursion (seaman/org-goto-heading-current) (end-of-line) (point))))
    (seaman/region-blank heading-eol (point))))

(defun seaman/org-heading-first-child ()
  (save-excursion
    (seaman/org-goto-heading-current)
    (let ((pos (seaman/get-point 'beginning-of-visual-line)))
      (org-backward-heading-same-level 1)
      (= pos (seaman/get-point 'beginning-of-visual-line)))))

(defun seaman/org-heading-has-children ()
  (interactive)
  (save-excursion (org-goto-first-child)))

(provide 'shapes-extension-org-queries)
;;; shapes-org-queries.el ends here
