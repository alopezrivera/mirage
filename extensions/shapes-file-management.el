;; Transform all files in directory from DOS to Unix line breaks
(defun custom/dos2unix (&optional dir)
  (let ((default-directory (or dir (file-name-directory buffer-file-name))))
    (shell-command "find . -maxdepth 1 -type f -exec dos2unix \\{\\} \\;")))

(defun custom/reload-from-disk (&optional buffer)
  "Revert BUFFER contents to the contents of its
file saved on disk, ignoring the auto-save file.
If the buffer has unsaved modifications, prompt
the user for confirmation."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (save-window-excursion
      (switch-to-buffer buffer)
      (if (not (buffer-modified-p))
	     (revert-buffer t t)
	   (revert-buffer t nil)))))

(global-set-key (kbd "C-c r") #'custom/reload-from-disk)
