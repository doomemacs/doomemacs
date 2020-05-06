;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

(use-package! lsp-java
  :after lsp-clients
  :preface
  (setq lsp-java-workspace-dir (concat doom-etc-dir "java-workspace"))
  (add-hook! java-mode-local-vars #'lsp!)
  :init
  (when (featurep! :tools debugger +lsp)
    (setq lsp-jt-root (concat lsp-java-server-install-dir "java-test/server/")
          dap-java-test-runner (concat lsp-java-server-install-dir "test-runner/junit-platform-console-standalone.jar"))
    (map! :map java-mode-map
          :localleader
          (:prefix ("t" . "Test")
           :desc "Run tests in class" "t" #'dap-java-run-test-class
           :desc "Run single test method" "s" #'dap-java-run-test-method))))
