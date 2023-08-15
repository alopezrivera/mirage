;; Swiper
(straight-use-package 'swiper)
(require 'swiper)

(defun seaman/swiper-multiple-cursors ()
  (interactive)
  (swiper-mc)
  (minibuffer-keyboard-quit))

;; M-RET: multiple-cursors-mode
(define-key swiper-map (kbd "M-<return>") #'seaman/swiper-multiple-cursors)

(defun seaman/swiper-isearch (orig-fun &rest args)
  "`swiper-isearch' the selected region. If none are, `swiper-isearch'."
  (if (region-active-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(deactivate-mark)
	(apply orig-fun (list (buffer-substring-no-properties beg end))))
    (apply orig-fun args)))

(advice-add 'swiper-isearch :around #'seaman/swiper-isearch)

(define-key global-map (kbd "C-s") #'swiper-isearch)

(provide 'seaman-module-swiper)
;;; seaman-swiper.el ends here
