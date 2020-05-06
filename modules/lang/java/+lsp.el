;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

(use-package! lsp-java
  :after lsp-clients
  :preface
  (setq lsp-java-server-install-dir (concat doom-etc-dir "eclipse.jdt.ls/server/")
        lsp-java-workspace-dir (concat doom-etc-dir "java-workspace")
        lsp-jt-root (concat doom-etc-dir "eclipse.jdt.ls/server/java-test/server/"))
  (add-hook! java-mode-local-vars #'lsp!)
  :config
  ;; TODO keybinds
  )
