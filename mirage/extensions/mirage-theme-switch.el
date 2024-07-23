(defun mirage/enable-or-load-theme (theme)
  (condition-case nil
      (enable-theme theme)
    (error (load-theme theme))))

(defvar mirage/enable-or-load-theme-hook nil
   "`load-theme' hook.")

(defun mirage/enable-or-load-theme-hook (&rest _args)
   "Run `load-theme-hook'."
   (run-hooks 'mirage/enable-or-load-theme-hook))

(advice-add 'enable-theme :after #'mirage/enable-or-load-theme-hook)
(advice-add 'load-theme   :after #'mirage/enable-or-load-theme-hook)

(defun mirage/theme-toggle ()
  "Toggle between `dark' and `light' themes
using `enable-theme'"
  (interactive)
  (let ((theme (nth 0 custom-enabled-themes)))
    (cond ((string-equal theme light-theme) (progn (disable-theme light-theme)
					           (mirage/enable-or-load-theme dark-theme)))
	  (t                                (progn (disable-theme dark-theme)
					           (mirage/enable-or-load-theme light-theme))))))

(global-set-key (kbd "C-t") #'mirage/theme-toggle)

(defcustom mirage/light-dark-themes '("modus"
                                      "nano")
  "Themes with light and dark versions.")

(defcustom mirage/theme-advice-dark '()
  "List of functions run when loading the `dark' theme, if it is ncluded in `mirage/light-dark-themes'.")

(defcustom mirage/theme-advice-light '()
  "List of functions run when loading the `light' theme, if it is ncluded in `mirage/light-dark-themes'.")

(defun mirage/theme-specific-advice (orig-fun &rest args)
  "Apply theme-specific advice when enabling themes, and
preserve modeline status through theme changes."
  (setq modeline-status mode-line-format)
  (apply orig-fun args)
  (let ((theme (nth 0 args)))
    (if (string-match-p (string-join mirage/light-dark-themes "\\|") (symbol-name theme))
        (let ((advice-list (if (string-equal theme light-theme)
                               mirage/theme-advice-light
                             mirage/theme-advice-dark)))
          (dolist (advice advice-list)
            (funcall advice))))
  (setq mode-line-format modeline-status)))

;; add
(advice-add 'enable-theme :around #'mirage/theme-specific-advice)
(advice-add 'load-theme   :around #'mirage/theme-specific-advice)

(defvar mirage/dark-line-number-colors '("#cfcfcf" "#262626")
  "Line number HTML colors for dark themes: FOREGROUND, BACKGROUND")

(defvar mirage/light-line-number-colors '("#878787" "#ededed")
  "Line number HTML colors for light themes: FOREGROUND, BACKGROUND")

(defun mirage/line-number-set-colors (fg bg)
  "Set the foreground (FG) and background (BG) colors of the line numbers
displayed by `display-line-numbers-mode'."
  (set-face-attribute 'line-number nil :foreground fg :background bg))

(defun mirage/set-dark-line-number-colors ()
  (apply 'mirage/line-number-set-colors mirage/dark-line-number-colors))

(defun mirage/set-light-line-number-colors ()
  (apply 'mirage/line-number-set-colors mirage/light-line-number-colors))

(add-to-list 'mirage/theme-advice-dark  #'mirage/set-dark-line-number-colors)
(add-to-list 'mirage/theme-advice-light #'mirage/set-light-line-number-colors)

;; reload active theme
(let ((active-theme (car custom-enabled-themes)))
  (if active-theme (enable-theme active-theme)))

(provide 'mirage-extension-theme-switch)
;;; mirage-theme-switch.el ends here
