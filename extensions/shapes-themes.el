;;; -*- lexical-binding: t; -*-

(defvar custom/load-theme-hook nil
   "`load-theme' hook.")

(defun custom/load-theme-hook (&rest _args)
   "Run `load-theme-hook'."
   (run-hooks 'custom/load-theme-hook))

(advice-add 'load-theme :after #'custom/load-theme-hook)

;; reload Org Mode
(defun custom/org-theme-reload ()
  (if (custom/in-mode "org-mode")
      (org-mode)
    (progn
      (setq window (get-buffer-window (current-buffer)))
      (cl-loop for buffer in (custom/visible-buffers)
	             collect (select-window (get-buffer-window buffer))
	 	     if (custom/in-mode "org-mode")
		        collect (org-mode))
      (select-window window))))

(add-hook 'custom/load-theme-hook #'custom/org-theme-reload)

(defun custom/theme-toggle ()
  "Toggle between `dark' and `light' themes
using `enable-theme'"
  (interactive)
  (let ((theme (nth 0 custom-enabled-themes)))
    (cond ((string-equal theme light) (progn (disable-theme light)
					        (load-theme    dark)))
	     (t                          (progn (disable-theme theme)
						(load-theme    light))))))

(global-set-key (kbd "C-t") 'custom/theme-toggle)

;; color
(defun custom/modeline-color (bg bg-in face face-in)
  "Set the color of the mode and header lines and blend the 
`doom-modeline-bar' with the background."
  (set-face-attribute 'mode-line          nil :foreground face    :background bg    :box nil)
  (set-face-attribute 'mode-line-inactive nil :foreground face-in :background bg-in :box nil)
  ;; header line
  (set-face-attribute 'header-line        nil :foreground face    :background bg    :box nil))

(defun custom/dark-modeline ()
  "Mode line for light themes."
  (custom/modeline-color "#3d3d3d" "#000000" "#cfcfcf" "#cfcfcf"))

(defun custom/light-modeline ()
  "Mode line for dark themes."
  (custom/modeline-color "#fff0ff" "#ededed" "#616161" "#878787"))

(defun custom/dark-line-numbers ()
  "Line numbers for light themes."
  (set-face-attribute 'line-number nil :foreground "#cfcfcf" :background "#262626"))

(defun custom/light-line-numbers ()
  "Line numbers for dark themes."
  (set-face-attribute 'line-number nil :foreground "#878787" :background "#ededed"))

(defun custom/light-advice ()
  (custom/light-modeline)
  (custom/light-line-numbers))

(defun custom/dark-advice ()
  (custom/dark-modeline)
  (custom/dark-line-numbers))

(defun custom/theme-specific-advice (orig-fun &rest args)
  "Apply theme-specific advice when enabling themes, and
preserve modeline status through theme changes."
  (setq modeline-status mode-line-format)
  (apply orig-fun args)
  (let ((theme (nth 0 args)))
    (if (string-match-p "modus\\|nano" (symbol-name theme))
	   (cond ((string-equal theme light) (custom/light-advice))
 		 ((string-equal theme dark)  (custom/dark-advice)))))
  (setq mode-line-format modeline-status))

;; enable-theme
(advice-add 'load-theme :around #'custom/theme-specific-advice)

(provide 'shapes-extensions-themes)
;;; shapes-themes.el ends here
