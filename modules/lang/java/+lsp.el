;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

(def-package! lsp-java
  :after-call java-mode
  :init (add-hook 'java-mode-local-vars-hook #'lsp!)
  :config
  (setq lsp-java-server-install-dir (concat doom-etc-dir "eclipse.jdt.ls/server/"))
  ;; TODO keybinds
  ;; TODO treemacs integration (?)
  )
