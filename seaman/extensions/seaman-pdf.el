(defun seaman/pdf-view-jump-to-top ()
  (interactive)
  (image-previous-line 9999))

(defun seaman/pdf-view-jump-to-bottom ()
  (interactive)
  (image-next-line 9999))

(define-key pdf-view-mode-map (kbd "<home>") #'seaman/pdf-view-jump-to-top)
(define-key pdf-view-mode-map (kbd "<end>")  #'seaman/pdf-view-jump-to-bottom)

(defun seaman/pdf-view-next-line (&optional arg)
  "Scroll upward by ARG lines. Stay in page."
  (interactive "P")
  (image-next-line (if arg arg 1)))

(defun seaman/pdf-view-previous-line (&optional arg)
  "Scroll upward by ARG lines. Stay in page."
  (interactive "P")
  (image-next-line (if arg (* -1 arg) -1)))

(define-key pdf-view-mode-map (kbd "<wheel-down>") #'seaman/pdf-view-next-line)
(define-key pdf-view-mode-map (kbd "<wheel-up>")   #'seaman/pdf-view-previous-line)
(define-key pdf-view-mode-map (kbd "C-<wheel-down>") (lambda () (interactive) (seaman/pdf-view-next-line 10)))
(define-key pdf-view-mode-map (kbd "C-<wheel-up>")   (lambda () (interactive) (seaman/pdf-view-previous-line 10)))

(defun seaman/pdf-view-fit-height ()
  (interactive)
  (setq pdf-view-display-size 'fit-page)
  (pdf-view-redisplay t)
  (pdf-view-center-in-window)
  (seaman/pdf-view-jump-to-top))

(defun seaman/pdf-view-fit-width ()
  (interactive)
  (setq pdf-view-display-size 'fit-width)
  (pdf-view-redisplay t)
  (pdf-view-center-in-window))

(define-key pdf-view-mode-map (kbd "<tab>") #'seaman/pdf-view-fit-height)
(define-key pdf-view-mode-map (kbd "C-<tab>") #'seaman/pdf-view-fit-width)

(defun seaman/pdf-refresh-themed-view ()
  (if pdf-view-themed-minor-mode
      (progn (pdf-view-themed-minor-mode -1)
             (pdf-view-themed-minor-mode))))

(add-hook 'seaman/enable-or-load-theme-hook #'seaman/pdf-refresh-themed-view)

(provide 'seaman-extension-pdf)
;;; seaman-pdf.el ends here
