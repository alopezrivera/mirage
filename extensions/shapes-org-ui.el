;;; -*- lexical-binding: t; -*-

;; theme reload advice
(defun custom/org-theme-reload ()
  "Re-set Org Mode UI typesetting after theme changes"
  (if (string-equal major-mode "org-mode")
      (org-mode)
    (progn
      (setq window (get-buffer-window (current-buffer)))
      (cl-loop for buffer in (custom/get-visible-buffers)
	             do (select-window (get-buffer-window buffer))
	 	     if (string-equal major-mode "org-mode")
                     do (custom/org-ui-typeset))
      (select-window window))))

(add-hook 'custom/enable-or-load-theme-hook #'custom/org-theme-reload)

;; UI typesetting
(defun custom/org-ui-typeset ()
  "Typeset the following Org Mode UI elements:
- title of Org Mode documents
- indent typeface used in `org-indent-mode' and `visual-line-mode'"
  (with-eval-after-load 'org-faces       (set-face-attribute 'org-document-title nil :font typeface-title :weight 'regular :height 200))
  (with-eval-after-load 'org-indent-mode (set-face-attribute 'org-indent         nil :inherit '(org-hide fixed-pitch))))

(add-hook 'org-mode-hook #'custom/org-ui-typeset)

(defface custom/variable-pitch-marker
  '((nil :inherit fixed-pitch))
  "List marker typeface.")

(defface custom/variable-pitch-indent
  '((nil :inherit fixed-pitch :invisible t))
  "Indent typeface.")

(defvar custom/variable-pitch-keywords '(("^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]\\{1\\}" 0 'custom/variable-pitch-marker)
					 ("^[[:blank:]]*[-+]\\{1\\}[[:blank:]]\\{1\\}"         0 'custom/variable-pitch-marker)
					 ("^[[:blank:]]+"                                  0 'custom/variable-pitch-indent))
  "Variable pitch font-lock keywords.")

(font-lock-add-keywords 'org-mode custom/variable-pitch-keywords 'append)

;; continuous numbering of Org Mode equations
(defun org-renumber-environment (orig-fun &rest args)
  (let ((results '()) 
        (counter -1)
        (numberp))

    (setq results (cl-loop for (begin .  env) in 
                        (org-element-map (org-element-parse-buffer) 'latex-environment
                          (lambda (env)
                            (cons
                             (org-element-property :begin env)
                             (org-element-property :value env))))
                        do
                        (cond
                         ((and (string-match "\\\\begin{equation}" env)
                               (not (string-match "\\\\tag{" env)))
                          (cl-incf counter)
                          (cons begin counter))
                         ((string-match "\\\\begin{align}" env)
                          (prog2
                              (cl-incf counter)
                              (cons begin counter)                          
                            (with-temp-buffer
                              (insert env)
                              (goto-char (point-min))
                              ;; \\ is used for a new line. Each one leads to a number
                              (cl-incf counter (count-matches "\\\\$"))
                              ;; unless there are nonumbers.
                              (goto-char (point-min))
                              (cl-decf counter (count-matches "\\nonumber")))))
                         (t
                          (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))
  
  (apply orig-fun args))

(advice-add 'org-create-formula-image :around #'org-renumber-environment)

(provide 'shapes-extension-org-ui)
;;; shapes-org-ui.el ends here
