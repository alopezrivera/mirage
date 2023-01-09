(defvar custom/org-babel-result-wrap-formats
  '(((assq :wrap (nth 2 info))
     (let* ((full (or (cdr (assq :wrap (nth 2 info))) "results"))
	    (split (split-string full))
	    (type (car split))
	   (opening-line (concat "#+begin_" full))
	   (closing-line (concat "#+end_" type)))
       (cond
        ;; Escape contents from "export" wrap.  Wrap
        ;; inline results within an export snippet with
        ;; appropriate value.
        ((eq t (compare-strings type nil nil "export" nil nil t))
	 (let ((backend (pcase split
			  (`(,_) "none")
			  (`(,_ ,b . ,_) b))))
	   (funcall wrap
		    opening-line closing-line
		    nil nil
		    (format "{{{results(@@%s:"
			    backend) "@@)}}}")))
        ;; Escape contents from "example" wrap.  Mark
        ;; inline results as verbatim.
	((eq t (compare-strings type nil nil "example" nil nil t))
	 (funcall wrap
		  opening-line closing-line
		  nil nil
		  "{{{results(=" "=)}}}"))
	;; Escape contents from "src" wrap.  Mark
	;; inline results as inline source code.
	((eq t (compare-strings type nil nil "src" nil nil t))
	 (let ((inline-open
		(pcase split
		  (`(,_)
		   "{{{results(src_none{")
		  (`(,_ ,language)
		   (format "{{{results(src_%s{" language))
		  (`(,_ ,language . ,rest)
		   (let ((r (mapconcat #'identity rest " ")))
		     (format "{{{results(src_%s[%s]{"
			     language r))))))
	   (funcall wrap
		    opening-line closing-line
		    nil nil
		    inline-open "})}}}")))
	;; Do not escape contents in non-verbatim
	;; blocks.  Return plain inline results.
	(t
	 (funcall wrap
		  opening-line closing-line
		  t nil
		  "{{{results(" ")}}}")))))
    ((member "html" result-params)
     (funcall wrap "#+begin_export html" "#+end_export" nil nil
	      "{{{results(@@html:" "@@)}}}"))
    ((member "latex" result-params)
     (funcall wrap "#+begin_export latex" "#+end_export" nil nil
	      "{{{results(@@latex:" "@@)}}}"))
    ((member "org" result-params)
     (goto-char beg) (when (org-at-table-p) (org-cycle))
     (funcall wrap "#+begin_src org" "#+end_src" nil nil
	      "{{{results(src_org{" "})}}}"))
    ((member "code" result-params)
     (let ((lang (or lang "none")))
       (funcall wrap (format "#+begin_src %s%s" lang results-switches)
		"#+end_src" nil nil
		(format "{{{results(src_%s[%s]{" lang results-switches)
		"})}}}")))
    ((member "raw" result-params)
     (goto-char beg) (when (org-at-table-p) (org-cycle)))
    ((or (member "drawer" result-params)
	 ;; Stay backward compatible with <7.9.2
	 (member "wrap" result-params))
     (goto-char beg) (when (org-at-table-p) (org-cycle))
     (funcall wrap ":results:" ":end:" 'no-escape nil
	      "{{{results(" ")}}}"))
    ((and inline (member "file" result-params))
     (funcall wrap nil nil nil nil "{{{results(" ")}}}"))
    ((and (not (funcall tabulablep result))
	  (not (member "file" result-params)))
     (let ((org-babel-inline-result-wrap
	    ;; Hard code {{{results(...)}}} on top of
	    ;; customization.
	    (format "{{{results(%s)}}}"
		    org-babel-inline-result-wrap)))
       (org-babel-examplify-region
	beg end results-switches inline)))))

(add-to-list 'custom/org-babel-result-wrap-formats
             '((member "latex-eq" result-params)
               (progn (funcall wrap "\\begin{equation}" "\\end{equation}" nil nil
	                       "{{{results(@@latex:" "@@)}}}"))
                      (org-latex-preview)))

(el-patch-feature org-babel)
(el-patch-defun org-babel-insert-result (result &optional result-params info hash lang exec-time)
  "Insert RESULT into the current buffer.

By default RESULT is inserted after the end of the current source
block.  The RESULT of an inline source block usually will be
wrapped inside a `results' macro and placed on the same line as
the inline source block.  The macro is stripped upon export.
Multiline and non-scalar RESULTS from inline source blocks are
not allowed.  When EXEC-TIME is provided it may be included in a
generated message.  With optional argument RESULT-PARAMS controls
insertion of results in the Org mode file.  RESULT-PARAMS can
take the following values:

replace - (default option) insert results after the source block
          or inline source block replacing any previously
          inserted results.

silent -- no results are inserted into the Org buffer but
          the results are echoed to the minibuffer and are
          ingested by Emacs (a potentially time consuming
          process).

none ---- no results are inserted into the Org buffer nor
          echoed to the minibuffer. they are not processed into
          Emacs-lisp objects at all.

file ---- the results are interpreted as a file path, and are
          inserted into the buffer using the Org file syntax.

list ---- the results are interpreted as an Org list.

raw ----- results are added directly to the Org file.  This is
          a good option if you code block will output Org
          formatted text.

drawer -- results are added directly to the Org file as with
          \"raw\", but are wrapped in a RESULTS drawer or results
          macro, allowing them to later be replaced or removed
          automatically.

org ----- results are added inside of a \"src_org{}\" or \"#+BEGIN_SRC
          org\" block depending on whether the current source block is
          inline or not.  They are not comma-escaped when inserted,
          but Org syntax here will be discarded when exporting the
          file.

html ---- results are added inside of a #+BEGIN_EXPORT HTML block
          or html export snippet depending on whether the current
          source block is inline or not.  This is a good option
          if your code block will output html formatted text.

latex --- results are added inside of a #+BEGIN_EXPORT LATEX
          block or latex export snippet depending on whether the
          current source block is inline or not.  This is a good
          option if your code block will output latex formatted
          text.

code ---- the results are extracted in the syntax of the source
          code of the language being evaluated and are added
          inside of a source block with the source-code language
          set appropriately.  Also, source block inlining is
          preserved in this case.  Note this relies on the
          optional LANG argument.

list ---- the results are rendered as a list.  This option not
          allowed for inline source blocks.

table --- the results are rendered as a table.  This option not
          allowed for inline source blocks.

INFO may provide the values of these header arguments (in the
`header-arguments-alist' see the docstring for
`org-babel-get-src-block-info'):

:file --- the name of the file to which output should be written.

:wrap --- the effect is similar to `latex' in RESULT-PARAMS but
          using the argument supplied to specify the export block
          or snippet type."
  (cond ((stringp result)
	 (setq result (org-no-properties result))
	 (when (member "file" result-params)
	   (setq result
                 (org-babel-result-to-file
		  result
		  (org-babel--file-desc (nth 2 info) result)
                  'attachment))))
	((listp result))
	(t (setq result (format "%S" result))))

  (if (and result-params (member "silent" result-params))
      (progn (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
	     result)
    (let ((inline (let ((context (org-element-context)))
		    (and (memq (org-element-type context)
			       '(inline-babel-call inline-src-block))
			 context))))
      (when inline
	(let ((warning
	       (or (and (member "table" result-params) "`:results table'")
		   (and result (listp result) "list result")
		   (and result (string-match-p "\n." result) "multiline result")
		   (and (member "list" result-params) "`:results list'"))))
	  (when warning
	    (user-error "Inline error: %s cannot be used" warning))))
      (save-excursion
	(let* ((visible-beg (point-min-marker))
	       (visible-end (copy-marker (point-max) t))
	       (inline (let ((context (org-element-context)))
			 (and (memq (org-element-type context)
				    '(inline-babel-call inline-src-block))
			      context)))
	       (existing-result (org-babel-where-is-src-block-result t nil hash))
	       (results-switches (cdr (assq :results_switches (nth 2 info))))
	       ;; When results exist outside of the current visible
	       ;; region of the buffer, be sure to widen buffer to
	       ;; update them.
	       (outside-scope (and existing-result
				   (buffer-narrowed-p)
				   (or (> visible-beg existing-result)
				       (<= visible-end existing-result))))
	       beg end indent)
	  ;; Ensure non-inline results end in a newline.
	  (when (and (org-string-nw-p result)
		     (not inline)
		     (not (string-equal (substring result -1) "\n")))
	    (setq result (concat result "\n")))
	  (unwind-protect
	      (progn
		(when outside-scope (widen))
		(if existing-result (goto-char existing-result)
		  (goto-char (org-element-property :end inline))
		  (skip-chars-backward " \t"))
		(unless inline
		  (setq indent (current-indentation))
		  (forward-line 1))
		(setq beg (point))
		(cond
		 (inline
		   ;; Make sure new results are separated from the
		   ;; source code by one space.
		   (unless existing-result
		     (insert " ")
		     (setq beg (point))))
		 ((member "replace" result-params)
		  (delete-region (point) (org-babel-result-end)))
		 ((member "append" result-params)
		  (goto-char (org-babel-result-end)) (setq beg (point-marker)))
		 ((member "prepend" result-params))) ; already there
		(setq results-switches
		      (if results-switches (concat " " results-switches) ""))
		(let ((wrap
		       (lambda (start finish &optional no-escape no-newlines
				      inline-start inline-finish)
			 (when inline
			   (setq start inline-start)
			   (setq finish inline-finish)
			   (setq no-newlines t))
			 (let ((before-finish (copy-marker end)))
			   (goto-char end)
			   (insert (concat finish (unless no-newlines "\n")))
			   (goto-char beg)
			   (insert (concat start (unless no-newlines "\n")))
			   (unless no-escape
			     (org-escape-code-in-region
			      (min (point) before-finish) before-finish))
			   (goto-char end))))
		      (tabulablep
		       (lambda (r)
			 ;; Non-nil when result R can be turned into
			 ;; a table.
                         (and (proper-list-p r)
			      (cl-every
                               (lambda (e) (or (atom e) (proper-list-p e)))
			       result)))))
		  ;; insert results based on type
		  (cond
		   ;; Do nothing for an empty result.
		   ((null result))
		   ;; Insert a list if preferred.
		   ((member "list" result-params)
		    (insert
		     (org-trim
		      (org-list-to-generic
		       (cons 'unordered
			     (mapcar
			      (lambda (e)
				(list (if (stringp e) e (format "%S" e))))
			      (if (listp result) result
				(split-string result "\n" t))))
		       '(:splicep nil :istart "- " :iend "\n")))
		     "\n"))
		   ;; Try hard to print RESULT as a table.  Give up if
		   ;; it contains an improper list.
		   ((funcall tabulablep result)
		    (goto-char beg)
		    (insert (concat (orgtbl-to-orgtbl
				     (if (cl-every
					  (lambda (e)
					    (or (eq e 'hline) (listp e)))
					  result)
					 result
				       (list result))
				     nil)
				    "\n"))
		    (goto-char beg)
		    (when (org-at-table-p) (org-table-align))
		    (goto-char (org-table-end)))
		   ;; Print verbatim a list that cannot be turned into
		   ;; a table.
		   ((listp result) (insert (format "%s\n" result)))
		   ((member "file" result-params)
		    (when inline
		      (setq result (org-macro-escape-arguments result)))
		    (insert result))
		   ((and inline (not (member "raw" result-params)))
		    (insert (org-macro-escape-arguments
			     (org-babel-chomp result "\n"))))
		   (t (goto-char beg) (insert result)))
		  (setq end (copy-marker (point) t))
		  ;; Possibly wrap result.
		  (el-patch-swap
                    (cond
		     ((assq :wrap (nth 2 info))
		      (let* ((full (or (cdr (assq :wrap (nth 2 info))) "results"))
			     (split (split-string full))
			     (type (car split))
			     (opening-line (concat "#+begin_" full))
			     (closing-line (concat "#+end_" type)))
		        (cond
		         ;; Escape contents from "export" wrap.  Wrap
		         ;; inline results within an export snippet with
		         ;; appropriate value.
		         ((eq t (compare-strings type nil nil "export" nil nil t))
			  (let ((backend (pcase split
					   (`(,_) "none")
					   (`(,_ ,b . ,_) b))))
			    (funcall wrap
				     opening-line closing-line
				     nil nil
				     (format "{{{results(@@%s:"
					     backend) "@@)}}}")))
		         ;; Escape contents from "example" wrap.  Mark
		         ;; inline results as verbatim.
		         ((eq t (compare-strings type nil nil "example" nil nil t))
			  (funcall wrap
				   opening-line closing-line
				   nil nil
				   "{{{results(=" "=)}}}"))
		         ;; Escape contents from "src" wrap.  Mark
		         ;; inline results as inline source code.
		         ((eq t (compare-strings type nil nil "src" nil nil t))
			  (let ((inline-open
			         (pcase split
				   (`(,_)
				    "{{{results(src_none{")
				   (`(,_ ,language)
				    (format "{{{results(src_%s{" language))
				   (`(,_ ,language . ,rest)
				    (let ((r (mapconcat #'identity rest " ")))
				      (format "{{{results(src_%s[%s]{"
					      language r))))))
			    (funcall wrap
				     opening-line closing-line
				     nil nil
				     inline-open "})}}}")))
		         ;; Do not escape contents in non-verbatim
		         ;; blocks.  Return plain inline results.
		         (t
			  (funcall wrap
				   opening-line closing-line
				   t nil
				   "{{{results(" ")}}}")))))
		     ((member "html" result-params)
		      (funcall wrap "#+begin_export html" "#+end_export" nil nil
			       "{{{results(@@html:" "@@)}}}"))
		     ((member "latex" result-params)
		      (funcall wrap "#+begin_export latex" "#+end_export" nil nil
			       "{{{results(@@latex:" "@@)}}}"))
		     ((member "org" result-params)
		      (goto-char beg) (when (org-at-table-p) (org-cycle))
		      (funcall wrap "#+begin_src org" "#+end_src" nil nil
			       "{{{results(src_org{" "})}}}"))
		     ((member "code" result-params)
		      (let ((lang (or lang "none")))
		        (funcall wrap (format "#+begin_src %s%s" lang results-switches)
			         "#+end_src" nil nil
			         (format "{{{results(src_%s[%s]{" lang results-switches)
			         "})}}}")))
		     ((member "raw" result-params)
		      (goto-char beg) (when (org-at-table-p) (org-cycle)))
		     ((or (member "drawer" result-params)
			  ;; Stay backward compatible with <7.9.2
			  (member "wrap" result-params))
		      (goto-char beg) (when (org-at-table-p) (org-cycle))
		      (funcall wrap ":results:" ":end:" 'no-escape nil
			       "{{{results(" ")}}}"))
		     ((and inline (member "file" result-params))
		      (funcall wrap nil nil nil nil "{{{results(" ")}}}"))
		     ((and (not (funcall tabulablep result))
			   (not (member "file" result-params)))
		      (let ((org-babel-inline-result-wrap
			     ;; Hard code {{{results(...)}}} on top of
			     ;; customization.
			     (format "{{{results(%s)}}}"
				     org-babel-inline-result-wrap)))
		        (org-babel-examplify-region
		         beg end results-switches inline))))
                  (let ((formats (copy-tree custom/org-babel-result-wrap-formats)))
                    (eval (add-to-list 'formats 'cond)))))
                ;; Possibly indent results in par with #+results line.
		(when (and (not inline) (numberp indent) (> indent 0)
			   ;; In this case `table-align' does the work
			   ;; for us.
			   (not (and (listp result)
				     (member "append" result-params))))
		  (indent-rigidly beg end indent))
                (let ((time-info
                       ;; Only show the time when something other than
                       ;; 0s will be shown, i.e. check if the time is at
                       ;; least half of the displayed precision.
                       (if (and exec-time (> (float-time exec-time) 0.05))
                           (format " (took %.1fs)" (float-time exec-time))
                         "")))
                  (if (null result)
                      (if (member "value" result-params)
                          (message "Code block returned no value%s." time-info)
                        (message "Code block produced no output%s." time-info))
                    (message "Code block evaluation complete%s." time-info))))
	    (set-marker end nil)
	    (when outside-scope (narrow-to-region visible-beg visible-end))
	    (set-marker visible-beg nil)
	    (set-marker visible-end nil)))))))

(provide 'shapes-extension-org-babel)
;;; shapes-org-babel.el ends here
