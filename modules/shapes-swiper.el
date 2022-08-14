;; Swiper
(straight-use-package 'swiper)
(require 'swiper)

(defun custom/swiper-isearch (orig-fun &rest args)
  "`swiper-isearch' the selected region. If none are, `swiper-isearch'."
  (if (region-active-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(deactivate-mark)
	(apply orig-fun (list (buffer-substring-no-properties beg end))))
    (apply orig-fun args)))

(advice-add 'swiper-isearch :around #'custom/swiper-isearch)

(define-key global-map (kbd "C-s") #'swiper-isearch)

(defun custom/narrow-and-search (beg end)
  "Narrow to region and trigger swiper search."
  (narrow-to-region beg end)
  (deactivate-mark)
  (swiper-isearch))

(defun custom/search-in-region (beg end)
  "Narrow and search active region. If the current
buffer is already narrowed, widen buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (not (buffer-narrowed-p))
      (if (and beg end)
	  (progn (custom/narrow-and-search beg end)))
    (progn (widen)
	   (if (bound-and-true-p multiple-cursors-mode)
	       (mc/disable-multiple-cursors-mode)))))

(defun custom/swiper-exit-narrow-search ()
  (interactive)
  (minibuffer-keyboard-quit)
  (if (buffer-narrowed-p)
      (widen)))

;; Narrow search
(define-key global-map (kbd "C-r") #'custom/search-in-region)

;; Exit narrow search from swiper
(define-key swiper-map (kbd "C-e") #'custom/swiper-exit-narrow-search)

(defun custom/swiper-multiple-cursors ()
  (interactive)
  (swiper-mc)
  (minibuffer-keyboard-quit))

;; M-RET: multiple-cursors-mode
(define-key swiper-map (kbd "M-<return>") #'custom/swiper-multiple-cursors)

(provide 'shapes-modules-swiper)
;;; shapes-swiper.el ends here
