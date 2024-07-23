(defun mirage/delete-line ()
  (delete-region (mirage/get-point 'beginning-of-line) (mirage/get-point 'end-of-line)))

(defun mirage/delete-word-forward (&optional arg)
  (interactive)
  (delete-region (point) (progn (forward-word arg) (point))))

(defun mirage/delete-word-backward (&optional arg)
  (interactive)
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-<delete>") #'mirage/delete-word-forward)
(global-set-key (kbd "C-<backspace>")  #'mirage/delete-word-backward)

(defun mirage/@delete-hungry (query)
  "Conditional region deletion.

Default: `delete-region'

If region starts at the beginning of an
indented line, delete region and indent.

If `query', delete the region and its indent 
plus one character."
  (setq beg (region-beginning) end (region-end))
  (if (mirage/at-indent beg)
	    (save-excursion (beginning-of-visual-line)
                      (if (and query (not (bobp)) (not (mirage/relative-line-empty -1)))
                          (left-char))
                      (delete-region (point) end))
    (delete-region beg end)))

(defun mirage/delete-hungry ()
  "If the region starts at the beginning of an 
indented line and the current mode is derived from 
`prog-mode',  delete the region and its indent plus 
one character."
  (interactive)
  (mirage/@delete-hungry (derived-mode-p 'prog-mode)))

(defun mirage/nimble-delete-forward ()
  "Conditional forward deletion.

Default: `delete-forward-char' 1

If next line is empty, forward delete indent of 
next line plus one character."
  (interactive)
  (cond ((and (eolp) (mirage/relative-line-indented 1)) (progn (setq beg (point)) (next-line) (back-to-indentation) (delete-region beg (point))))
	    ((mirage/relative-line-empty)                   (delete-region (point) (mirage/get-point 'next-line)))
	    (t                                              (delete-forward-char 1))))

(global-set-key (kbd "<delete>") #'mirage/nimble-delete-forward)

(defun mirage/nimble-delete-backward ()
  "Conditional forward deletion.

Default: `delete-backward-char' 1

If `multiple-cursors-mode' is active, `delete-backward-char' 1.

If region is active, delete region.

If cursor lies either `mirage/at-indent' or is preceded only by
whitespace, delete region from `point' to `beginning-of-visual-line'."
  (interactive)
  (if (not (bound-and-true-p multiple-cursors-mode))
      (cond ((and (region-active-p) (not (mirage/region-blank))) (mirage/delete-hungry))
	    ((mirage/at-indent)                                  (delete-region (point) (mirage/get-point 'beginning-of-visual-line)))
	    (t                                                   (delete-backward-char 1)))
    (delete-backward-char 1)))

(global-set-key (kbd "<backspace>") #'mirage/nimble-delete-backward)

;; Increase kill ring size
(setq kill-ring-max 200)

(defun mirage/kill-ring-mouse ()
  "If a region is active, save the region to the
kill ring. Otherwise, yank the last entry in the
kill ring."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (yank)))

(global-set-key   (kbd "<mouse-3>") #'mirage/kill-ring-mouse)
(global-unset-key (kbd "<down-mouse-3>"))

;; Unset secondary overlay key bindings
(global-unset-key [M-mouse-1])
(global-unset-key [M-drag-mouse-1])
(global-unset-key [M-mouse-3])
(global-unset-key [M-mouse-2])

(defun mirage/smart-comment ()
  "If a region is active, comment out all lines in the
region. Otherwise, comment out current line if it is
not empty. In any case, advance to next line."
  (interactive)
  (let (beg end)
    ;; If a region is active
    (if (region-active-p)
	      ;; If the beginning and end of the region are in
	      ;; the same line, select entire line
	      (if (= (count-lines (region-beginning) (region-end)) 1)
		  (setq beg (line-beginning-position) end (line-end-position))
		;; Else, select region from the start of its first
		;; line to the end of its last.
		(setq beg (save-excursion (goto-char (region-beginning)) (line-beginning-position))
		      end (save-excursion (goto-char (region-end)) (line-end-position))))
      ;; Else, select line
      (setq beg (line-beginning-position) end (line-end-position)))

    ;; Comment or uncomment region
    ;; If Org Mode is active
    (if (not (mirage/relative-line-empty))
	      (comment-or-uncomment-region beg end))
    ;; Move to the beginning of the next line
    (beginning-of-line-text 2)))

(global-set-key (kbd "C-x ;") #'mirage/smart-comment)

;; Ensure rectangular-region-mode is loaded
(require 'rectangular-region-mode)

;; Multiple cursor rectangle definition mouse event
(defun mirage/mouse-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (set-rectangular-region-anchor)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(global-set-key (kbd "M-<down-mouse-1>") #'mirage/mouse-rectangle)

;; Enter multiple-cursors-mode
(defun mirage/rectangular-region-multiple-cursors ()
  (interactive)
  (rectangular-region-mode 0)
  (multiple-cursors-mode 1)
  (deactivate-mark)
  (mc/for-each-fake-cursor
   (if (invisible-p (marker-position (overlay-get cursor 'point)))
       (mc/remove-fake-cursor cursor))))

(define-key rectangular-region-mode-map (kbd "<return>") #'mirage/rectangular-region-multiple-cursors)

;; Exit rectangular-region-mode
(define-key rectangular-region-mode-map (kbd "<escape>") #'rrm/keyboard-quit)
(define-key rectangular-region-mode-map (kbd "<mouse-1>") #'rrm/keyboard-quit)

(provide 'mirage-extension-editing)
;;; mirage-editing.el ends here
