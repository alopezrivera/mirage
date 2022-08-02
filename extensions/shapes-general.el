(defun custom/regex-match-count (regexp str)
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

(defun custom/eolp (orig-fun &rest args)
  (interactive)
  (or (apply orig-fun args) (looking-at-p "[[:blank:]]*$")))

(advice-add 'eolp :around #'custom/eolp)

(defun custom/at-point (go-to-point &optional point)
  (let ((point (or point (point))))
    (save-excursion
      (funcall go-to-point)
      (= point (point)))))

(defun custom/at-indent (&optional point)
  (and (custom/relative-line-indented) (custom/at-point 'back-to-indentation point)))

(defun custom/relative-line (query &optional number &rest args)
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

(defun custom/relative-line-regex (pattern &optional number)
  (let ((number (or number 0)))
    (save-excursion
      (beginning-of-line-text (+ number 1))
      (setq line (buffer-substring-no-properties (custom/get-point 'beginning-of-line) (custom/get-point 'end-of-line))))
    (string-match-p pattern line)))

(defun custom/relative-line-list (&optional number)
  (custom/relative-line-regex "^[[:blank:]]*\\([0-9]+[.\\)]\\{1\\}\\|[-+*]\\{1\\}\\)[[:blank:]]+.*$" number))

(defun custom/relative-line-empty (&optional number)
  (custom/relative-line-regex "^[[:space:]]*$" number))

(defun custom/relative-line-wrapped ()
  (> (custom/get-point 'beginning-of-visual-line) (custom/get-point 'beginning-of-line-text)))

(defun custom/relative-line-indented (&optional number)
  (custom/relative-line-regex "^[[:blank:]]+.*$" number))

(defun custom/relative-line-list-ordered (&optional number)
  (custom/relative-line-regex "^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]+.*$" number))

(defun custom/relative-line-list-unordered (&optional number)
  (custom/relative-line-regex "^[[:blank:]]*[-+*]\\{1\\}[[:blank:]]+.*$" number))

(defun custom/region-blank (&optional beg end)
  (let ((beg (or beg (region-beginning)))
	      (end (or end (region-end))))
    (setq region (buffer-substring-no-properties beg end))
    (string-match "\\`[[:space:]]*\\'$" region)))

(defun custom/region-multiline-visual ()
  "Return t if a region is active and spans more than one visual line."
  (and (region-active-p) (> (custom/region-count-visual-lines) 1)))

(defun custom/region-count-visual-lines ()
  "Count visual lines in an active region."
  (interactive)
  (save-excursion 
    (beginning-of-visual-line)
    (count-screen-lines (region-beginning) (region-end))))

(defun custom/in-mode (mode)
  "Return t if mode is currently active."
  (string-equal major-mode mode))

;; Retrieve current theme
(defun custom/current-theme ()
  (substring (format "%s" (nth 0 custom-enabled-themes))))

(defun custom/current-window-number ()
  "Retrieve the current window's number."
  (setq window (prin1-to-string (get-buffer-window (current-buffer))))
  (string-match "^[^0-9]*\\([0-9]+\\).*$" window)
  (match-string 1 window))

(defun custom/find-buffer-by-file-name (file)
  (cl-loop for buffer in (buffer-list)
	        if (string-equal (buffer-name buffer) (file-name-nondirectory file))
	           return buffer
		finally return nil))

(defun custom/get-point (command &rest args)
  (interactive)
  (save-excursion
    (apply command args)
    (point)))

(defun custom/last-change ()
  "Retrieve last change in current buffer."
  (setq last-change (nth 1 buffer-undo-list))
  (let ((beg (car last-change))
        (end (cdr last-change)))
    (buffer-substring-no-properties beg end)))

(defun custom/visible-buffers ()
  (cl-delete-duplicates (mapcar #'window-buffer (window-list))))

(defun custom/get-keyword-key-value (kwd)
   (let ((data (cadr kwd)))
     (list (plist-get data :key)
           (plist-get data :value))))

(defmacro custom/@buffers (command &optional buffers)
  (let ((buffers (or buffers (buffer-list))))
    `(cl-loop for buffer in ',buffers
              collect (save-window-excursion (switch-to-buffer buffer)
                                             ,command))))

(defun <> (a b c)
  (and (> b a) (> c b)))
