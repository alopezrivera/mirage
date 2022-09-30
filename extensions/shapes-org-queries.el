;;; -*- lexical-binding: t; -*-

;; `org-in-src-block-p' gives false positives as of Org Mode 9.5.3. For
;; this reason, determine if cursor in src block with the more reliable
;; `org-babel-where-is-src-block-head'
(advice-add 'org-in-src-block-p :override #'org-babel-where-is-src-block-head)

(defun custom/org-at-ellipsis (&optional position)
  (or (custom/org-at-ellipsis-h position) (custom/org-at-ellipsis-l position)))

(defun custom/org-at-ellipsis-l (&optional position)
  (and (custom/org-relative-line-list-folded) (custom/at-point 'end-of-visual-line)))

(defun custom/org-at-ellipsis-h (&optional position) 
  (and (custom/org-relative-line-heading-folded) (custom/at-point 'end-of-visual-line)))

(defun custom/org-at-keyword (&optional number)
  (custom/relative-line-regex "^#+.*$" number))

(defun custom/org-at-heading (&optional point)
  (let ((point (or point (point))))
    (save-excursion (goto-char point) (custom/org-relative-line-heading))))

(defun custom/org-at-bol-list () 
  (and (custom/org-relative-line-list) (custom/at-point 'beginning-of-line-text)))

(defun custom/org-at-bol-heading () 
  (and (custom/org-relative-line-heading) (custom/at-point 'custom/org-goto-heading-bol)))

(defun custom/org-at-eol-heading ()
  (and (custom/org-relative-line-heading) (eolp) (not (custom/org-at-ellipsis-h)) (not (custom/org-relative-line-heading-empty))))

(defun custom/org-after-list-or-indent ()
  (or (custom/org-relative-line-list -1) (custom/relative-line-indented -1)))

(defun custom/org-relative-line-list (&optional number)
  (custom/relative-line (lambda () (progn (beginning-of-line-text) (org-at-item-p)))  number))

(defun custom/org-relative-line-heading (&optional number)
  (custom/relative-line 'org-at-heading-p number))

(defun custom/org-relative-line-paragraph (&optional number)
  "Determine whether the current line -or the NUMBER'th line relative to it
is an indented paragraph."
  (let ((number (or number 0)))
    (and (not (custom/org-relative-line-heading number))
	       (not (custom/org-relative-line-list    number))
	       (not (org-in-src-block-p))
	       (custom/relative-line-indented number)
	       (or  (custom/org-relative-line-list      (- number 1))
		    (custom/org-relative-line-paragraph (- number 1))))))

(defun custom/org-relative-line-list-empty (&optional number)
  (and (custom/org-relative-line-list)
       (or (custom/relative-line-regex "^[[:blank:]]*[-+*]\\{1\\}[[:blank:]]+$" number)
	         (custom/relative-line-regex "^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]+$" number))))

(defun custom/org-relative-line-list-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded list item."
  (custom/relative-line (lambda () (and (org-at-item-p) (invisible-p (point-at-eol)))) number))

(defun custom/org-relative-line-heading-empty (&optional number)
  (custom/relative-line (lambda () (beginning-of-line-text) (org-point-at-end-of-empty-headline)) number))

(defun custom/org-relative-line-heading-folded (&optional number)
  "Returns non-nil if `point-at-eol' of current visual line
is on a folded heading."
  (custom/relative-line (lambda () (and (org-at-heading-p) (invisible-p (point-at-eol)))) number))

(defun custom/org-relative-line-heading-or-list (&optional number)
  (custom/relative-line 'org-at-heading-or-item-p number))

(defun custom/org-subtree-blank ()
  "Return t if the current subtree consists of
a `custom/region-blank'."
  (interactive)
  (apply #'custom/region-blank (custom/org-get-subtree-region)))

(defun custom/org-subtree-empty ()
  (interactive)
  (string-equal "" (custom/org-get-subtree-content)))

(defun custom/org-headings-follow ()
  (let ((pos (custom/get-point 'beginning-of-visual-line)))
    (save-excursion (custom/org-goto-heading-next)
		           (and (not (= pos (point))) (custom/org-relative-line-heading)))))

(defun custom/org-headings-precede ()
  (let ((pos (custom/get-point 'beginning-of-visual-line)))
    (save-excursion (custom/org-goto-heading-previous)
		          (and (not (= pos (point))) (custom/org-relative-line-heading)))))

(defun custom/org-subtree-blank-up-to-point ()
  (interactive)
  (let ((heading-eol (save-excursion (custom/org-goto-heading-current) (end-of-line) (point))))
    (custom/region-blank heading-eol (point))))

(defun custom/org-heading-first-child ()
  (save-excursion
    (custom/org-goto-heading-current)
    (let ((pos (custom/get-point 'beginning-of-visual-line)))
      (org-backward-heading-same-level 1)
      (= pos (custom/get-point 'beginning-of-visual-line)))))

(defun custom/org-heading-has-children ()
  (interactive)
  (save-excursion (org-goto-first-child)))

(provide 'shapes-extension-org-queries)
;;; shapes-org-queries.el ends here
