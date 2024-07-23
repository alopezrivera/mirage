;; Swiper
(straight-use-package 'swiper)
(require 'swiper)

(defun mirage/swiper-multiple-cursors ()
  (interactive)
  (swiper-mc)
  (minibuffer-keyboard-quit))

;; M-RET: multiple-cursors-mode
(define-key swiper-map (kbd "M-<return>") #'mirage/swiper-multiple-cursors)

(defun mirage/swiper-isearch (orig-fun &rest args)
  "`swiper-isearch' the selected region. If none are, `swiper-isearch'."
  (if (region-active-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(deactivate-mark)
	(apply orig-fun (list (buffer-substring-no-properties beg end))))
    (apply orig-fun args)))

(advice-add 'swiper-isearch :around #'mirage/swiper-isearch)

(define-key global-map (kbd "C-s") #'swiper-isearch)

(provide 'mirage-module-swiper)
;;; mirage-swiper.el ends here
