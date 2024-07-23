(defun mirage/pdf-view-jump-to-top ()
  (interactive)
  (image-previous-line 9999))

(defun mirage/pdf-view-jump-to-bottom ()
  (interactive)
  (image-next-line 9999))

(define-key pdf-view-mode-map (kbd "<home>") #'mirage/pdf-view-jump-to-top)
(define-key pdf-view-mode-map (kbd "<end>")  #'mirage/pdf-view-jump-to-bottom)

(defun mirage/pdf-view-next-line (&optional arg)
  "Scroll upward by ARG lines. Stay in page."
  (interactive "P")
  (image-next-line (if arg arg 1)))

(defun mirage/pdf-view-previous-line (&optional arg)
  "Scroll upward by ARG lines. Stay in page."
  (interactive "P")
  (image-next-line (if arg (* -1 arg) -1)))

(define-key pdf-view-mode-map (kbd "<wheel-down>") #'mirage/pdf-view-next-line)
(define-key pdf-view-mode-map (kbd "<wheel-up>")   #'mirage/pdf-view-previous-line)
(define-key pdf-view-mode-map (kbd "C-<wheel-down>") (lambda () (interactive) (mirage/pdf-view-next-line 10)))
(define-key pdf-view-mode-map (kbd "C-<wheel-up>")   (lambda () (interactive) (mirage/pdf-view-previous-line 10)))

(defun mirage/pdf-view-fit-height ()
  (interactive)
  (setq pdf-view-display-size 'fit-page)
  (pdf-view-redisplay t)
  (pdf-view-center-in-window)
  (mirage/pdf-view-jump-to-top))

(defun mirage/pdf-view-fit-width ()
  (interactive)
  (setq pdf-view-display-size 'fit-width)
  (pdf-view-redisplay t)
  (pdf-view-center-in-window))

(define-key pdf-view-mode-map (kbd "<tab>") #'mirage/pdf-view-fit-height)
(define-key pdf-view-mode-map (kbd "C-<tab>") #'mirage/pdf-view-fit-width)

(defun mirage/pdf-refresh-themed-view ()
  (if pdf-view-themed-minor-mode
      (progn (pdf-view-themed-minor-mode -1)
             (pdf-view-themed-minor-mode))))

(add-hook 'mirage/enable-or-load-theme-hook #'mirage/pdf-refresh-themed-view)

(provide 'mirage-extension-pdf)
;;; mirage-pdf.el ends here
