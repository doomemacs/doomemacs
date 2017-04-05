;;; ../core/autoload/test.el

;;;###autoload
(defmacro def-test-group! (name &rest body)
  (declare (indent defun))
  (dolist (form body)
    (when (eq (car form) 'ert-deftest)
      (setf (cadr form) (intern (format "test-%s-%s" name (symbol-name (cadr form)))))))
  `(progn ,@body))
