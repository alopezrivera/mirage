;;; -*- lexical-binding: t; -*-

(defmacro custom/@buffers (command &optional buffers)
  (let ((buffers (or buffers (buffer-list))))
    `(cl-loop for buffer in ',buffers
              collect (if (buffer-live-p buffer)
                          (save-window-excursion (switch-to-buffer buffer)
                                                 ,command)))))

(provide 'shapes-extension-execution)
;;; shapes-execution.el ends here
