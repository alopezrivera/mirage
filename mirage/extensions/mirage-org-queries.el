;; `org-in-src-block-p' gives false positives as of Org Mode 9.5.3. For
;; this reason, determine if cursor in src block with the more reliable
;; `org-babel-where-is-src-block-head'
(defun mirage/org-in-src-block-p (orig-fun &rest args)
  (if args
      (apply orig-fun args)
    (org-babel-where-is-src-block-head)))

(advice-add 'org-in-src-block-p :around #'mirage/org-in-src-block-p)

(defun mirage/org-at-ellipsis (&optional position)
  (or (mirage/org-at-ellipsis-h position) (mirage/org-at-ellipsis-l position)))

(defun mirage/org-at-ellipsis-l (&optional position)
  (and (mirage/org-relative-line-list-folded) (mirage/at-point 'end-of-visual-line)))

(defun mirage/org-at-ellipsis-h (&optional position) 
  (and (mirage/org-relative-line-heading-folded) (mirage/at-point 'end-of-visual-line)))

(defun mirage/org-at-keyword (&optional number)
  (mirage/relative-line-regex "^#+.*$" number))

(defun mirage/org-at-heading (&optional point)
  (let ((point (or point (point))))
    (save-excursion (goto-char point) (mirage/org-relative-line-heading))))

(defun mirage/org-at-bol-list () 
  (and (mirage/org-relative-line-list) (mirage/at-point 'beginning-of-line-text)))

(defun mirage/org-at-bol-heading () 
  (and (mirage/org-relative-line-heading) (mirage/at-point 'mirage/org-goto-heading-bol)))

(defun mirage/org-at-eol-heading ()
  (and (mirage/org-relative-line-heading) (eolp) (not (mirage/org-at-ellipsis-h)) (not (mirage/org-relative-line-heading-empty))))

(defun mirage/org-after-list-or-indent ()
  (or (mirage/org-relative-line-list -1) (mirage/relative-line-indented -1)))

(defun mirage/org-relative-line-list (&optional number)
  (mirage/relative-line (lambda () (progn (beginning-of-line-text) (org-at-item-p)))  number))

(defun mirage/org-relative-line-heading (&optional number)
  (mirage/relative-line 'org-at-heading-p number))

(defun mirage/org-relative-line-paragraph (&optional number)
  "Determine whether the current line -or the NUMBER'th line relative to it
is an indented paragraph."
  (let ((number (or number 0)))
    (and (not (mirage/org-relative-line-heading number))
	       (not (mirage/org-relative-line-list    number))
	       (not (org-in-src-block-p))
	       (mirage/relative-line-indented number)
	       (or  (mirage/org-relative-line-list      (- number 1))
		    (mirage/org-relative-line-paragraph (- number 1))))))

(defun mirage/org-relative-line-list-empty (&optional number)
  (and (mirage/org-relative-line-list)
       (or (mirage/relative-line-regex "^[[:blank:]]*[-+*]\\{1\\}[[:blank:]]+$" number)
	         (mirage/relative-line-regex "^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]+$" number))))

(defun mirage/org-relative-line-list-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded list item."
  (mirage/relative-line (lambda () (and (org-at-item-p) (invisible-p (point-at-eol)))) number))

(defun mirage/org-relative-line-heading-empty (&optional number)
  (mirage/relative-line (lambda () (beginning-of-line-text) (org-point-at-end-of-empty-headline)) number))

(defun mirage/org-relative-line-heading-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded heading."
  (mirage/relative-line (lambda () (and (org-at-heading-p) (invisible-p (point-at-eol)))) number))

(defun mirage/org-relative-line-heading-or-list (&optional number)
  (mirage/relative-line 'org-at-heading-or-item-p number))

(defun mirage/org-subtree-blank ()
  "Return t if the current subtree consists of
a `mirage/region-blank'."
  (interactive)
  (apply 'mirage/region-blank (mirage/org-get-subtree-region)))

(defun mirage/org-subtree-empty ()
  (interactive)
  (string-equal "" (mirage/org-get-subtree-content)))

(defun mirage/org-headings-follow ()
  (let ((pos (mirage/get-point 'beginning-of-visual-line)))
    (save-excursion (mirage/org-goto-heading-next)
		           (and (not (= pos (point))) (mirage/org-relative-line-heading)))))

(defun mirage/org-headings-precede ()
  (let ((pos (mirage/get-point 'beginning-of-visual-line)))
    (save-excursion (mirage/org-goto-heading-previous)
		          (and (not (= pos (point))) (mirage/org-relative-line-heading)))))

(defun mirage/org-subtree-blank-up-to-point ()
  (interactive)
  (let ((heading-eol (save-excursion (mirage/org-goto-heading-current) (end-of-line) (point))))
    (mirage/region-blank heading-eol (point))))

(defun mirage/org-heading-first-child ()
  (save-excursion
    (mirage/org-goto-heading-current)
    (let ((pos (mirage/get-point 'beginning-of-visual-line)))
      (org-backward-heading-same-level 1)
      (= pos (mirage/get-point 'beginning-of-visual-line)))))

(defun mirage/org-heading-has-children ()
  (interactive)
  (save-excursion (org-goto-first-child)))

(provide 'mirage-extension-org-queries)
;;; mirage-org-queries.el ends here
