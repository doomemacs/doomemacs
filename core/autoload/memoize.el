;;; memoize.el
(provide 'doom-lib-memoize)

;;;###autoload
(defvar doom-memoized-table (make-hash-table :test 'equal :size 10)
  "A lookup table containing memoized functions. The keys are argument lists,
and the value is the function's return value.")

(defsubst doom--memoize-wrap (name)
  "Return the memoized version of FUNC."
  (let ((func (symbol-function name)))
    `(lambda (&rest args)
       ,(documentation name)
       (let ((key (cons ',name args)))
         (or (gethash key doom-memoized-table)
             (puthash key (apply ',func args) doom-memoized-table))))))

;;;###autoload
(defmacro def-memoized! (name arglist &rest body)
  "Create a memoize'd function. NAME, ARGLIST, DOCSTRING and BODY
have the same meaning as in `defun'."
  (declare (indent defun) (doc-string 3))
  (when (stringp (car body))
    (setcar body (concat (car body) " (memoized)")))
  `(progn
     (defun ,name ,arglist ,@body)
     (fset ',name (doom--memoize-wrap ',name))))

