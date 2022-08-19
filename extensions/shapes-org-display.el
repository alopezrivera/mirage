;;; -*- lexical-binding: t; -*-

(defface custom/variable-pitch-marker
  '((nil :inherit 'fixed-pitch))
  "List marker typeface.")

(defface custom/variable-pitch-indent
  '((nil :inherit 'fixed-pitch :invisible t))
  "Indent typeface.")

(defvar custom/variable-pitch-keywords '(("^[[:blank:]]*[0-9]+[.\\)]\\{1\\}[[:blank:]]\\{1\\}" 0 'custom/variable-pitch-marker)
					     ("^[[:blank:]]*[-+]\\{1\\}[[:blank:]]\\{1\\}"         0 'custom/variable-pitch-marker)
					     ("^[[:blank:]]+"                                      0 'custom/variable-pitch-indent))
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
                        collect
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

(provide 'shapes-extension-org-display)
;;; shapes-org-display.el ends here
