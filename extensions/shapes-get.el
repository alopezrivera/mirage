;;; -*- lexical-binding: t; -*-

;; Retrieve current theme
(defun custom/get-active-theme ()
  (substring (format "%s" (nth 0 custom-enabled-themes))))

(defun custom/get-point (command &rest args)
  (interactive)
  (save-excursion
    (apply command args)
    (point)))

(defun custom/get-last-change ()
  "Retrieve last change in current buffer."
  (setq last-change (nth 1 buffer-undo-list))
  (let ((beg (car last-change))
        (end (cdr last-change)))
    (buffer-substring-no-properties beg end)))

(defun custom/count-substrings (regexp str)
  "Return the number of substrings of STR matching REGEXP"
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

(defun custom/get-keyword-key-value (kwd)
   (let ((data (cadr kwd)))
     (list (plist-get data :key)
           (plist-get data :value))))

(defun custom/get-visible-buffers ()
  (cl-delete-duplicates (mapcar #'window-buffer (window-list))))

(defun custom/get-buffer-by-file-name (file)
  (cl-loop for buffer in (buffer-list)
	   if (string-equal (buffer-name buffer) (file-name-nondirectory file))
	   return buffer
	   finally return nil))

(defun custom/get-active-window-number ()
  "Retrieve the current window's number."
  (setq window (prin1-to-string (get-buffer-window (current-buffer))))
  (string-match "^[^0-9]*\\([0-9]+\\).*$" window)
  (match-string 1 window))

(defvar custom/keymap-list '()
  "List containing the symbols of all keymaps in the `obarray'.")

(defun custom/get-keymaps ()
  "Return a list containing the symbols of all keymaps in the `obarray'."
  (mapatoms (lambda (m) (if (condition-case nil
                                (or (keymapp (symbol-value m))
                                    (keymapp m))
                              (error nil))
                            (add-to-list 'custom/keymap-list m))
          obarray))
  (when (called-interactively-p 'interactive)
        (message "Keymap list updated, %s keymaps found" (length custom/keymap-list)))
  custom/keymap-list)

(defun custom/get-keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

(provide 'shapes-extension-get)
;;; shapes-get.el ends here
