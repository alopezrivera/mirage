;;; -*- lexical-binding: t; -*-

(defun custom/enable-or-load-theme (theme)
  (condition-case nil
      (enable-theme theme)
    (error (load-theme theme))))

(defvar custom/enable-or-load-theme-hook nil
   "`load-theme' hook.")

(defun custom/enable-or-load-theme-hook (&rest _args)
   "Run `load-theme-hook'."
   (run-hooks 'custom/enable-or-load-theme-hook))

(advice-add 'enable-theme :after #'custom/enable-or-load-theme-hook)
(advice-add 'load-theme   :after #'custom/enable-or-load-theme-hook)

(defun custom/theme-toggle ()
  "Toggle between `dark' and `light' themes
using `enable-theme'"
  (interactive)
  (let ((theme (nth 0 custom-enabled-themes)))
    (cond ((string-equal theme light) (progn (disable-theme light)
					          (custom/enable-or-load-theme    dark)))
	       (t                          (progn (disable-theme theme)
					          (custom/enable-or-load-theme    light))))))

(global-set-key (kbd "C-t") #'custom/theme-toggle)

(defcustom custom/light-dark-themes '("modus"
                                      "nano")
  "Themes with light and dark versions.")

(defcustom custom/theme-advice-dark '()
  "List of functions run when loading the `dark' theme, if it is included in `custom/light-dark-themes'.")

(defcustom custom/theme-advice-light '()
  "List of functions run when loading the `light' theme, if it is included in `custom/light-dark-themes'.")

(defun custom/theme-specific-advice (orig-fun &rest args)
  "Apply theme-specific advice when enabling themes, and
preserve modeline status through theme changes."
  (setq modeline-status mode-line-format)
  (apply orig-fun args)
  (let ((theme (nth 0 args)))
    (if (string-match-p (string-join custom/light-dark-themes "\\|") (symbol-name theme))
        (let ((advice-list (if (string-equal theme light)
                               custom/theme-advice-light
                             custom/theme-advice-dark)))
          (dolist (advice advice-list)
            (funcall advice))))
  (setq mode-line-format modeline-status)))

;; add
(advice-add 'enable-theme :around #'custom/theme-specific-advice)
(advice-add 'load-theme   :around #'custom/theme-specific-advice)

(defvar custom/dark-line-number-colors '("#cfcfcf" "#262626")
  "Line number HTML colors for dark themes: FOREGROUND, BACKGROUND")

(defvar custom/light-line-number-colors '("#878787" "#ededed")
  "Line number HTML colors for light themes: FOREGROUND, BACKGROUND")

(defun custom/line-number-set-colors (fg bg)
  "Set the foreground (FG) and background (BG) colors of the line numbers
displayed by `display-line-numbers-mode'."
  (set-face-attribute 'line-number nil :foreground fg :background bg))

(defun custom/set-dark-line-number-colors ()
  (apply 'custom/line-number-set-colors custom/dark-line-number-colors))

(defun custom/set-light-line-number-colors ()
  (apply 'custom/line-number-set-colors custom/dark-line-number-colors))

(add-to-list 'custom/theme-advice-dark  #'custom/set-dark-line-number-colors)
(add-to-list 'custom/theme-advice-light #'custom/set-light-line-number-colors)

;; reload active theme
(let ((active-theme (car custom-enabled-themes)))
  (if active-theme (enable-theme active-theme)))

(provide 'shapes-extension-theme-switch)
;;; shapes-theme-switch.el ends here
