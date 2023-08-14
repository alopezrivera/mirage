(defun seaman/enable-or-load-theme (theme)
  (condition-case nil
      (enable-theme theme)
    (error (load-theme theme))))

(defvar seaman/enable-or-load-theme-hook nil
   "`load-theme' hook.")

(defun seaman/enable-or-load-theme-hook (&rest _args)
   "Run `load-theme-hook'."
   (run-hooks 'seaman/enable-or-load-theme-hook))

(advice-add 'enable-theme :after #'seaman/enable-or-load-theme-hook)
(advice-add 'load-theme   :after #'seaman/enable-or-load-theme-hook)

(defun seaman/theme-toggle ()
  "Toggle between `dark' and `light' themes
using `enable-theme'"
  (interactive)
  (let ((theme (nth 0 custom-enabled-themes)))
    (cond ((string-equal theme light-theme) (progn (disable-theme light-theme)
					           (seaman/enable-or-load-theme dark-theme)))
	  (t                                (progn (disable-theme dark-theme)
					           (seaman/enable-or-load-theme light-theme))))))

(global-set-key (kbd "C-t") #'seaman/theme-toggle)

(defcustom seaman/light-dark-themes '("modus"
                                      "nano")
  "Themes with light and dark versions.")

(defcustom seaman/theme-advice-dark '()
  "List of functions run when loading the `dark' theme, if it is ncluded in `seaman/light-dark-themes'.")

(defcustom seaman/theme-advice-light '()
  "List of functions run when loading the `light' theme, if it is ncluded in `seaman/light-dark-themes'.")

(defun seaman/theme-specific-advice (orig-fun &rest args)
  "Apply theme-specific advice when enabling themes, and
preserve modeline status through theme changes."
  (setq modeline-status mode-line-format)
  (apply orig-fun args)
  (let ((theme (nth 0 args)))
    (if (string-match-p (string-join seaman/light-dark-themes "\\|") (symbol-name theme))
        (let ((advice-list (if (string-equal theme light-theme)
                               seaman/theme-advice-light
                             seaman/theme-advice-dark)))
          (dolist (advice advice-list)
            (funcall advice))))
  (setq mode-line-format modeline-status)))

;; add
(advice-add 'enable-theme :around #'seaman/theme-specific-advice)
(advice-add 'load-theme   :around #'seaman/theme-specific-advice)

(defvar seaman/dark-line-number-colors '("#cfcfcf" "#262626")
  "Line number HTML colors for dark themes: FOREGROUND, BACKGROUND")

(defvar seaman/light-line-number-colors '("#878787" "#ededed")
  "Line number HTML colors for light themes: FOREGROUND, BACKGROUND")

(defun seaman/line-number-set-colors (fg bg)
  "Set the foreground (FG) and background (BG) colors of the line numbers
displayed by `display-line-numbers-mode'."
  (set-face-attribute 'line-number nil :foreground fg :background bg))

(defun seaman/set-dark-line-number-colors ()
  (apply 'seaman/line-number-set-colors seaman/dark-line-number-colors))

(defun seaman/set-light-line-number-colors ()
  (apply 'seaman/line-number-set-colors seaman/light-line-number-colors))

(add-to-list 'seaman/theme-advice-dark  #'seaman/set-dark-line-number-colors)
(add-to-list 'seaman/theme-advice-light #'seaman/set-light-line-number-colors)

;; reload active theme
(let ((active-theme (car custom-enabled-themes)))
  (if active-theme (enable-theme active-theme)))

(provide 'shapes-extension-theme-switch)
;;; shapes-theme-switch.el ends here
