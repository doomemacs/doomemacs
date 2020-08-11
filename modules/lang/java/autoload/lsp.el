;;; lang/java/autoload/lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! :tools debugger +lsp)

;;;###autoload
(defun +java/run-test ()
  "Runs test at point.
If in a method, runs the test method, otherwise runs the entire test class."
  (interactive)
  (require 'dap-java)
  (condition-case nil
      (dap-java-run-test-method)
    (user-error (dap-java-run-test-class))))

;;;###autoload
(defun +java/debug-test ()
  "Runs test at point in a debugger.
If in a method, runs the test method, otherwise runs the entire test class."
  (interactive)
  (require 'dap-java)
  (condition-case nil
      (call-interactively #'dap-java-debug-test-method)
    (user-error (call-interactively #'dap-java-debug-test-class))))
