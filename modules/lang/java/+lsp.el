;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

(use-package! lsp-java
  :after lsp-clients
  :preface
  (setq lsp-java-workspace-dir (concat doom-etc-dir "java-workspace"))
  (add-hook! java-mode-local-vars #'lsp!)
  (when (featurep! :tools debugger +lsp)
    (defun +java/run-test ()
      "Runs test at point. If in a method, runs the test method, otherwise runs the entire test class."
      (interactive)
      (condition-case nil
          (dap-java-run-test-method)
        (user-error (dap-java-run-test-class))))

    (defun +java/debug-test ()
      "Runs test at point in a debugger. If in a method, runs the test method, otherwise runs the entire test class."
      (interactive)
      (condition-case nil
          (call-interactively #'dap-java-debug-test-method)
        (user-error (call-interactively #'dap-java-debug-test-class))))

    (map! :map java-mode-map
          :localleader
          (:prefix ("t" . "Test")
           :desc "Run test class or method" "t" #'+java/run-test
           :desc "Run all tests in class" "a" #'dap-java-run-test-class
           :desc "Debug test class or method" "d" #'+java/debug-test
           :desc "Debug all tests in class" "D" #'dap-java-debug-test-class)))
  :init
  (when (featurep! :tools debugger +lsp)
    (setq lsp-jt-root (concat lsp-java-server-install-dir "java-test/server/")
          dap-java-test-runner (concat lsp-java-server-install-dir "test-runner/junit-platform-console-standalone.jar"))))
