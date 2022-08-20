(defvar custom/cases #s(hash-table
                        test equal)
  "Hash table the keys of which are each function for which cases have
been defined, and the items of which are lists containing the name
of each case which has been defined for a given function")

(defun cases (orig-fun cases)
  "Cases"
  (setq-local case-list (gethash orig-fun custom/cases))
  (cl-loop for case in cases
           collect (let ((advice (intern (concat (symbol-name orig-fun) "--case--" (nth 0 case)))))
                     (setf (symbol-function advice)
                           `(lambda (orig-fun &rest args)
                               (if ,(nth 1 case)
                                   ,(nth 2 case)
                                 (apply orig-fun args))))
                     (advice-add orig-fun :around advice)
                     ;; record
                     (setq-local case-list
                                 (if case-list
                                     (add-to-list 'case-list advice t)
                                   (list advice))))
           finally (puthash orig-fun case-list custom/cases)))

(defun case-get (function)
  "Retrieve all cases of FUNCTION from its entry in `custom/cases' hash table"
  (gethash function custom/cases))

(defun case-remove (function &optional n)
  "Remove all cases of FUNCTION, that is, remove their advice on FUNCTION and
their name from the `custom/cases' hash table entry of FUNCTION.
If N is provided, remove the Nth case of FUNCTION in the same fashion"
  (setq-local case-list (case-get function))
  (setq-local element   (if n (nth n case-list) nil))
  (if element
      (advice-remove function element)
    (cl-loop for case in case-list
             collect (advice-remove function case)))
  (puthash function (if element (remove element case-list) nil) custom/cases))
