;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (and (featurep! +lsp) (not (featurep! :tools lsp +eglot)))

(use-package! lsp-java
  :after lsp-mode
  :preface
  (setq lsp-java-workspace-dir (concat doom-etc-dir "java-workspace"))
  (add-hook 'java-mode-local-vars-hook #'lsp!)
  :config
  (when (featurep! :tools debugger +lsp)
    (setq lsp-jt-root (concat lsp-java-server-install-dir "java-test/server/")
          dap-java-test-runner (concat lsp-java-server-install-dir "test-runner/junit-platform-console-standalone.jar"))))


(use-package! dap-java
  :when (featurep! :tools debugger +lsp)
  :commands dap-java-run-test-class dap-java-debug-test-class
  :init
  (map! :after cc-mode ; where `java-mode' is defined
        :map java-mode-map
        :localleader
        (:prefix ("t" . "Test")
         :desc "Run test class or method"   "t" #'+java/run-test
         :desc "Run all tests in class"     "a" #'dap-java-run-test-class
         :desc "Debug test class or method" "d" #'+java/debug-test
         :desc "Debug all tests in class"   "D" #'dap-java-debug-test-class)))
