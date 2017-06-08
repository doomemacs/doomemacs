;;; core/autoload/test.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro def-test-group! (name &rest body)
  (declare (indent defun))
  (unless (plist-get body :disabled)
    (dolist (form body)
      (when (eq (car form) 'ert-deftest)
        (setf (cadr form) (intern (format "test-%s-%s" name (symbol-name (cadr form)))))))
    `(progn ,@body)))
