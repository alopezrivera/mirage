;; accent typefaces
(defvar seaman/accents '(seaman/italic))

(defun seaman/theme-accents (orig-fun &rest args)
  "Many themes will override certain face *attributes*, such as `italic'. To prevent
this, this function loops over all accent typefaces in `seaman/accents', which contains
faces (defined with `defface') named ~seaman/<attribute>~, and makes the ~<attribute>~
inherit from ~seaman/<attribute>~.

As such, when this function is run, the `italic' face attribute will be made to
inherit from `seaman/italic' as in the expression below.

   (set-face-attribute 'italic nil :inherit 'seaman/italic)

Thus, our preferred accent typefaces will stand whatever harassment they may be put
through as a theme loads."
  ;; load theme
  (apply orig-fun args)
  ;; restore accents
  (cl-loop for accent in seaman/accents
	   do (let ((face (intern (car (last (split-string (symbol-name accent) "/"))))))
		     (set-face-attribute face nil :inherit accent))))

(advice-add 'seaman/enable-or-load-theme :around #'seaman/theme-accents)

(defun seaman/quit-window ()
  (interactive)
  (if current-prefix-arg
      (quit-window)
    (quit-window 1)))

(with-eval-after-load 'helpful
  (cl-loop for map in '(help-mode-map
                        helpful-mode-map)
           do (define-key (symbol-value map) [remap quit-window] #'seaman/quit-window)))

(defun seaman/window-resize (width)
  (window-resize nil (- width (window-width)) t))

(defun seaman/window-resize-fraction (fr &optional min)
  "Resize window to a fraction of the frame width."
  (interactive)
  (let ((width (max (if min min 0) (truncate (* fr (frame-width))))))
    (window-resize nil (- width (window-width)) t)))

;; Record last sent message
(defvar last-message nil)
(defadvice message (after my-message pre act) (setq last-message ad-return-value))

(defun seaman/undefined-override (orig-fun &rest args)
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
(advice-add 'undefined :around #'seaman/undefined-override)

(defcustom seaman/mode-line nil
  "Variable containing the format of the hidden mode line")

(defcustom seaman/header-line nil
  "Variable containing the format of the hidden header line")

(defun seaman/hide-mode-line ()
  "Hide `modeline' in current buffer"
  (interactive)
  (let ((m mode-line-format)
        (h header-line-format))
       (seaman/@buffers (if (or m h)
                            (progn (setq seaman/mode-line   m)
                                   (setq seaman/header-line h)
                                   (setq mode-line-format   nil)
                                   (setq header-line-format nil))
                          (progn (setq mode-line-format seaman/mode-line)
                                 (setq header-line-format seaman/header-line))))))

(global-set-key (kbd "M-m") #'seaman/hide-mode-line)

(defun seaman/mode-line-invert ()
  (interactive)
  (if mode-line-format
      (seaman/@buffers (progn (set 'header-line-format mode-line-format)
                              (set 'mode-line-format nil)))
    (seaman/@buffers (progn (set 'mode-line-format header-line-format)
                            (set 'header-line-format nil)))))

(global-set-key (kbd "M-t") #'seaman/mode-line-invert)

(provide 'shapes-extension-ui)
;;; shapes-ui.el ends here
