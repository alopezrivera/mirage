(defmacro seaman/@buffers (command &optional buffers)
  (let ((buffers (or buffers (buffer-list))))
    `(cl-loop for buffer in ',buffers
              do (if (buffer-live-p buffer)
                     (save-window-excursion
                       (switch-to-buffer buffer)
                       ,command)))))

(provide 'seaman-extension-execution)
;;; seaman-execution.el ends here
