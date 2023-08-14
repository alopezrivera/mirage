;; yasnippet
(straight-use-package 'yasnippet)

;; (yas-global-mode 1)

(defun seaman/<-snippet (orig-fun &rest args)
  "Require < before snippets."
  (interactive)
  (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	(if (not (string-equal line ""))
	    (if (string-equal (substring line 0 1) "<")
		(progn (save-excursion (move-beginning-of-line nil)
				       (right-char 1)
				       (delete-region (line-beginning-position) (point)))
		       (apply orig-fun args)))))

(advice-add 'yas-expand :around #'seaman/<-snippet)

;; yasnippet-snippets
(straight-use-package 'yasnippet-snippets)

(provide 'shapes-module-yasnippet)
;;; shapes-yasnippet.el ends here
