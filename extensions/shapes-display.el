(defun custom/window-resize (width)
  (window-resize nil (- width (window-width)) t))

(defun custom/window-resize-fraction (fr &optional min)
  "Resize window to a fraction of the frame width."
  (interactive)
  (let ((width (max (if min min 0) (truncate (* fr (frame-width))))))
    (window-resize nil (- width (window-width)) t)))

(defcustom custom/mode-line nil
  "Variable containing the format of the hidden mode line")

(defcustom custom/header-line nil
  "Variable containing the format of the hidden header line")

(defun custom/hide-mode-line ()
  "Hide `modeline' in current buffer"
  (interactive)
  (let ((m mode-line-format)
        (h header-line-format))
       (custom/@buffers (if (or m h)
                            (progn (setq custom/mode-line   m)
                                   (setq custom/header-line h)
                                   (setq mode-line-format   nil)
                                   (setq header-line-format nil))
                          (progn (setq mode-line-format custom/mode-line)
                                 (setq header-line-format custom/header-line))))))

(global-set-key (kbd "M-m") #'custom/hide-mode-line)

(defun custom/variable-replace (a b)
  "Set the value of `b' to that of `a', and
that of `a' to nil in all buffers"
  (let ((line (symbol-value a)))
    (custom/@buffers (progn (set b line)
                            (set a nil)))))

(defun custom/mode-line-invert ()
  (interactive)
  (let ((m 'mode-line-format)
        (h 'header-line-format))
    (if mode-line-format
        (custom/variable-replace m h)
      (custom/variable-replace h m))))

(global-set-key (kbd "M-t") #'custom/mode-line-invert)

;; Record last sent message
(defvar last-message nil)
(defadvice message (after my-message pre act) (setq last-message ad-return-value))

(defun custom/undefined-override (orig-fun &rest args)
  "Override `undefined' function to suppress
undefined key binding messages when interrupting
key binding input with C-g."
  (let ((inhibit-message t)
	    (message-log-max nil))
    (progn (apply orig-fun args)
	       (setq _message last-message)))
  (if (string-match-p (regexp-quote "C-g is undefined") _message)
      (keyboard-quit)
    (message _message)))

;; Override the undefined key binding notice with a keyboard-quit
(advice-add 'undefined :around #'custom/undefined-override)
