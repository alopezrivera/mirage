(defun custom/pdf-view-next-line (&optional arg)
  "Scroll upward by ARG lines. Stay in page."
  (interactive "P")
  (image-next-line (if arg arg 1)))

(defun custom/pdf-view-previous-line (&optional arg)
  "Scroll upward by ARG lines. Stay in page."
  (interactive "P")
  (image-next-line (if arg (* -1 arg) -1)))

(define-key pdf-view-mode-map (kbd "<wheel-down>") #'custom/pdf-view-next-line)
(define-key pdf-view-mode-map (kbd "<wheel-up>")   #'custom/pdf-view-previous-line)

(defun custom/pdf-refresh-themed-view ()
  (pdf-view-themed-minor-mode -1)
  (pdf-view-themed-minor-mode))

(add-hook 'custom/enable-or-load-theme-hook #'custom/pdf-refresh-themed-view)

(defun custom/pdf-view-restore-zoom ()
  (interactive)
  (setq pdf-view-display-size 'fit-page)
  (pdf-view-redisplay t)
  (pdf-view-center-in-window))

(define-key pdf-view-mode-map (kbd "<tab>") #'custom/pdf-view-restore-zoom)

(provide 'shapes-extension-pdf)
;;; shapes-pdf.el ends here
