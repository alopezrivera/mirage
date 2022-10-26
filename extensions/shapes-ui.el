;;; -*- lexical-binding: t; -*-

;; accent typefaces
(defvar custom/accents '(custom/italic))

(defun custom/theme-accents (orig-fun &rest args)
  "Many themes will override certain face *attributes*, such as `italic'. To prevent
this, this function loops over all accent typefaces in `custom/accents', which contains
faces (defined with `defface') named ~custom/<attribute>~, and makes the ~<attribute>~
inherit from ~custom/<attribute>~.

As such, when this function is run, the `italic' face attribute will be made to
inherit from `custom/italic' as in the expression below.

   (set-face-attribute 'italic nil :inherit 'custom/italic)

Thus, our preferred accent typefaces will stand whatever harassment they may be put
through as a theme loads."
  ;; load theme
  (apply orig-fun args)
  ;; restore accents
  (cl-loop for accent in custom/accents
	   do (let ((face (intern (car (last (split-string (symbol-name accent) "/"))))))
		     (set-face-attribute face nil :inherit accent))))

(advice-add 'load-theme :around #'custom/theme-accents)

(defun custom/quit-window ()
  (interactive)
  (if current-prefix-arg
      (quit-window)
    (quit-window 1)))

(with-eval-after-load 'helpful
  (cl-loop for map in '(help-mode-map
                        helpful-mode-map)
           do (define-key (symbol-value map) [remap quit-window] #'custom/quit-window)))

(defun custom/window-resize (width)
  (window-resize nil (- width (window-width)) t))

(defun custom/window-resize-fraction (fr &optional min)
  "Resize window to a fraction of the frame width."
  (interactive)
  (let ((width (max (if min min 0) (truncate (* fr (frame-width))))))
    (window-resize nil (- width (window-width)) t)))

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

(defun custom/mode-line-invert ()
  (interactive)
  (if mode-line-format
      (custom/@buffers (progn (set 'header-line-format mode-line-format)
                              (set 'mode-line-format nil)))
    (custom/@buffers (progn (set 'mode-line-format header-line-format)
                            (set 'header-line-format nil)))))

(global-set-key (kbd "M-t") #'custom/mode-line-invert)

(provide 'shapes-extension-ui)
;;; shapes-ui.el ends here
