;;; memoize.el
(provide 'core-lib-memoize)

(defvar doom-memoized-table (make-hash-table :test 'equal)
  "TODO")

;;;###autoload
(defun doom-memoize (func)
  "Memoize the given function."
  (cl-typecase func
    (symbol (put func 'function-documentation
                 (concat (documentation func) " (memoized)"))
            (fset func (doom--memoize-wrap (symbol-function func)))
            func)
    (function (doom--memoize-wrap func))))

;;;###autoload
(defun doom--memoize-wrap (func)
  "Return the memoized version of FUNC."
  `(lambda (&rest args)
     (or (gethash args doom-memoized-table)
         (puthash args (apply ',func args) doom-memoized-table))))

;;;###autoload
(defmacro def-memoized! (name arglist &rest body)
  "Create a memoize'd function. NAME, ARGLIST, DOCSTRING and BODY
have the same meaning as in `defun'."
  (declare (indent defun) (doc-string 3))
  `(progn
     (defun ,name ,arglist ,@body)
     (doom-memoize ',name)))

