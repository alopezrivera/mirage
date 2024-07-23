(defmacro mirage/@buffers (command &optional buffers)
  (let ((buffers (or buffers (buffer-list))))
    `(cl-loop for buffer in ',buffers
              do (if (buffer-live-p buffer)
                     (save-window-excursion
                       (switch-to-buffer buffer)
                       ,command)))))

(provide 'mirage-extension-execution)
;;; mirage-execution.el ends here
