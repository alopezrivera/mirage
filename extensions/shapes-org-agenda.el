(require 'cal-iso)

(defun custom/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
  (let* ((dayname (calendar-day-name date))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month))
	 (year (nth 2 date))
         ;; extra information
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " W%02d" iso-week)
		       ""))
         ;; label
         (label-length 27)
         (label (format "%-9s %2d %s"
                        dayname day monthname year))
         ;; margin fill
         (fill (make-string (/ (- label-length (string-width label)) 2) 32))
         ;; separators
         (sep-l (concat ">>" fill))
         (sep-r (concat fill "<<"))
         ;; highlights
         (highlight-l (make-string (- (/ (window-width) 2)
                                      (/ (string-width label) 2)
                                      (string-width sep-l)) 9472))
         (highlight-r (make-string (- (/ (window-width) 2)
                                      (/ (string-width label) 2)
                                      (string-width sep-r)) 9472)))
    (concat highlight-l sep-l label sep-r highlight-r)))

(setq org-agenda-format-date 'custom/org-agenda-format-date-aligned)

;; Mark items as done
(defun custom/org-agenda-todo-done ()
  (interactive)
  (org-agenda-todo 'done))

(define-key org-agenda-mode-map (kbd "d") 'custom/org-agenda-todo-done)

(provide 'shapes-extension-org-agenda)
;;; shapes-org-agenda.el ends here
