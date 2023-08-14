(defun seaman/eolp (orig-fun &rest args)
  (interactive)
  (or (apply orig-fun args) (looking-at-p "[[:blank:]]*$")))

(advice-add 'eolp :around #'seaman/eolp)

(defun seaman/at-point (go-to-point &optional point)
  (let ((point (or point (point))))
    (save-excursion
      (funcall go-to-point)
      (= point (point)))))

(defun seaman/at-indent (&optional point)
  (and (seaman/relative-line-indented) (seaman/at-point 'back-to-indentation point)))

(defun seaman/relative-line (query &optional number &rest args)
  "Return the result of a boolean query at the beginning
of the current visual line, or another specified by its
relative position to the current line.
Optionally, `args' may be given as input to be passed
to the query at execution."
  (let ((number (or number 0)))
    (save-excursion
      (beginning-of-visual-line)
      (beginning-of-line-text (+ number 1))
      (apply query args))))

(defun seaman/relative-line-regex (pattern &optional number)
  (let ((number (or number 0)))
    (save-excursion
      (beginning-of-line-text (+ number 1))
      (setq line (buffer-substring-no-properties (seaman/get-point 'beginning-of-line) (seaman/get-point 'end-of-line))))
    (string-match-p pattern line)))

(defun seaman/relative-line-list (&optional number)
  (seaman/relative-line-regex "^[[:blank:]]*\\([0-9]+[.\\)]\\{1\\}\\|[-+*]\\{1\\}\\)[[:blank:]]+.*$" number))

(defun seaman/relative-line-empty (&optional number)
  (seaman/relative-line-regex "^[[:space:]]*$" number))

(defun seaman/relative-line-wrapped ()
  (> (seaman/get-point 'beginning-of-visual-line) (seaman/get-point 'beginning-of-line-text)))

(defun seaman/relative-line-indented (&optional number)
  (seaman/relative-line-regex "^[[:blank:]]+.*$" number))

(defun seaman/relative-line-list-ordered (&optional number)
  (seaman/relative-line-regex "^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]+.*$" number))

(defun seaman/relative-line-list-unordered (&optional number)
  (seaman/relative-line-regex "^[[:blank:]]*[-+*]\\{1\\}[[:blank:]]+.*$" number))

(defun seaman/region-blank (&optional beg end)
  (let ((beg (or beg (region-beginning)))
	      (end (or end (region-end))))
    (setq region (buffer-substring-no-properties beg end))
    (string-match "\\`[[:space:]]*\\'$" region)))

(defun seaman/region-multiline-visual ()
  "Return t if a region is active and spans more than one visual line."
  (and (region-active-p) (> (seaman/region-count-visual-lines) 1)))

(defun seaman/region-count-visual-lines ()
  "Count visual lines in an active region."
  (interactive)
  (save-excursion 
    (beginning-of-visual-line)
    (count-screen-lines (region-beginning) (region-end))))

(provide 'shapes-extension-queries)
;;; shapes-queries.el ends here
