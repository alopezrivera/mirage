(defun custom/pdf-view-jump-to-top ()
  (interactive)
  (image-previous-line 9999))

(defun custom/pdf-view-jump-to-bottom ()
  (interactive)
  (image-next-line 9999))

(define-key pdf-view-mode-map (kbd "<home>") #'custom/pdf-view-jump-to-top)
(define-key pdf-view-mode-map (kbd "<end>")  #'custom/pdf-view-jump-to-bottom)

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
(define-key pdf-view-mode-map (kbd "C-<wheel-down>") (lambda () (interactive) (custom/pdf-view-next-line 10)))
(define-key pdf-view-mode-map (kbd "C-<wheel-up>")   (lambda () (interactive) (custom/pdf-view-previous-line 10)))

(defun custom/pdf-view-fit-height ()
  (interactive)
  (setq pdf-view-display-size 'fit-page)
  (pdf-view-redisplay t)
  (pdf-view-center-in-window)
  (custom/pdf-view-jump-to-top))

(defun custom/pdf-view-fit-width ()
  (interactive)
  (setq pdf-view-display-size 'fit-width)
  (pdf-view-redisplay t)
  (pdf-view-center-in-window))

(define-key pdf-view-mode-map (kbd "<tab>") #'custom/pdf-view-fit-height)
(define-key pdf-view-mode-map (kbd "C-<tab>") #'custom/pdf-view-fit-width)

(defun custom/pdf-refresh-themed-view ()
  (if pdf-view-themed-minor-mode
      (progn (pdf-view-themed-minor-mode -1)
             (pdf-view-themed-minor-mode))))

(add-hook 'custom/enable-or-load-theme-hook #'custom/pdf-refresh-themed-view)

(provide 'shapes-extension-pdf)
;;; shapes-pdf.el ends here
