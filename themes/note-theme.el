;;; untitled-theme.el --- untitled
;;; Version: 1.0
;;; Commentary:
;;; A theme called untitled
;;; Code:

(deftheme untitled "DOCSTRING for untitled")
  (custom-theme-set-faces 'untitled
   '(default ((t (:foreground "#232323" :background "#ffffff" ))))
   '(cursor ((t (:background "#000000" ))))
   '(fringe ((t (:background "#f6f4f4" ))))
   '(mode-line ((t (:foreground "#3a3a3a" :background "#f6f6f6" ))))
   '(region ((t (:background "#000000" ))))
   '(secondary-selection ((t (:background "#3e3834" ))))
   '(font-lock-builtin-face ((t (:foreground "#a970c5" ))))
   '(font-lock-comment-face ((t (:foreground "#619567" ))))
   '(font-lock-function-name-face ((t (:foreground "#bb2626" ))))
   '(font-lock-keyword-face ((t (:foreground "#848484" ))))
   '(font-lock-string-face ((t (:foreground "#26bb88" ))))
   '(font-lock-type-face ((t (:foreground "#46111f" ))))
   '(font-lock-constant-face ((t (:foreground "#a86577" ))))
   '(font-lock-variable-name-face ((t (:foreground "#7088b0" ))))
   '(minibuffer-prompt ((t (:foreground "#444444" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "red" :bold t ))))
   )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'untitled)

;;; untitled-theme.el ends here
