(defun mirage/eolp (orig-fun &rest args)
  (interactive)
  (or (apply orig-fun args) (looking-at-p "[[:blank:]]*$")))

(advice-add 'eolp :around #'mirage/eolp)

(defun mirage/at-point (go-to-point &optional point)
  (let ((point (or point (point))))
    (save-excursion
      (funcall go-to-point)
      (= point (point)))))

(defun mirage/at-indent (&optional point)
  (and (mirage/relative-line-indented) (mirage/at-point 'back-to-indentation point)))

(defun mirage/relative-line (query &optional number &rest args)
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

(defun mirage/relative-line-regex (pattern &optional number)
  (let ((number (or number 0)))
    (save-excursion
      (beginning-of-line-text (+ number 1))
      (setq line (buffer-substring-no-properties (mirage/get-point 'beginning-of-line) (mirage/get-point 'end-of-line))))
    (string-match-p pattern line)))

(defun mirage/relative-line-list (&optional number)
  (mirage/relative-line-regex "^[[:blank:]]*\\([0-9]+[.\\)]\\{1\\}\\|[-+*]\\{1\\}\\)[[:blank:]]+.*$" number))

(defun mirage/relative-line-empty (&optional number)
  (mirage/relative-line-regex "^[[:space:]]*$" number))

(defun mirage/relative-line-wrapped ()
  (> (mirage/get-point 'beginning-of-visual-line) (mirage/get-point 'beginning-of-line-text)))

(defun mirage/relative-line-indented (&optional number)
  (mirage/relative-line-regex "^[[:blank:]]+.*$" number))

(defun mirage/relative-line-list-ordered (&optional number)
  (mirage/relative-line-regex "^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]+.*$" number))

(defun mirage/relative-line-list-unordered (&optional number)
  (mirage/relative-line-regex "^[[:blank:]]*[-+*]\\{1\\}[[:blank:]]+.*$" number))

(defun mirage/region-blank (&optional beg end)
  (let ((beg (or beg (region-beginning)))
	      (end (or end (region-end))))
    (setq region (buffer-substring-no-properties beg end))
    (string-match "\\`[[:space:]]*\\'$" region)))

(defun mirage/region-multiline-visual ()
  "Return t if a region is active and spans more than one visual line."
  (and (region-active-p) (> (mirage/region-count-visual-lines) 1)))

(defun mirage/region-count-visual-lines ()
  "Count visual lines in an active region."
  (interactive)
  (save-excursion 
    (beginning-of-visual-line)
    (count-screen-lines (region-beginning) (region-end))))

(provide 'mirage-extension-queries)
;;; mirage-queries.el ends here
