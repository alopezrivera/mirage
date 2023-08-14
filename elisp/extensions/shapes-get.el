;; Retrieve current theme
(defun seaman/get-active-theme ()
  (substring (format "%s" (nth 0 custom-enabled-themes))))

(defun seaman/get-point (command &rest args)
  (interactive)
  (save-excursion
    (apply command args)
    (point)))

(defun seaman/get-last-change ()
  "Retrieve last change in current buffer."
  (setq last-change (nth 1 buffer-undo-list))
  (let ((beg (car last-change))
        (end (cdr last-change)))
    (buffer-substring-no-properties beg end)))

(defun seaman/count-substrings (regexp str)
  "Return the number of substrings of STR matching REGEXP"
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

(defun seaman/get-keyword-key-value (kwd)
   (let ((data (cadr kwd)))
     (list (plist-get data :key)
           (plist-get data :value))))

(defun seaman/get-visible-buffers ()
  (cl-delete-duplicates (mapcar #'window-buffer (window-list))))

(defun seaman/get-buffer-by-file-name (file)
  (cl-loop for buffer in (buffer-list)
	   if (string-equal (buffer-name buffer) (file-name-nondirectory file))
	   return buffer
	   finally return nil))

(defun seaman/get-active-window-number ()
  "Retrieve the current window's number."
  (setq window (prin1-to-string (get-buffer-window (current-buffer))))
  (string-match "^[^0-9]*\\([0-9]+\\).*$" window)
  (match-string 1 window))

(defvar seaman/keymap-list '()
  "List containing the symbols of all keymaps in the `obarray'.")

(defun seaman/get-keymaps ()
  "Return a list containing the symbols of all keymaps in the `obarray'."
  (mapatoms (lambda (m) (if (condition-case nil
                                (or (keymapp (symbol-value m))
                                    (keymapp m))
                              (error nil))
                            (add-to-list 'seaman/keymap-list m))
          obarray))
  (when (called-interactively-p 'interactive)
        (message "Keymap list updated, %s keymaps found" (length seaman/keymap-list)))
  seaman/keymap-list)

(defun seaman/get-keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

(provide 'shapes-extension-get)
;;; shapes-get.el ends here
