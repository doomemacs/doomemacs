;;; lang/java/+lsp.el -*- lexical-binding: t; -*-
;;;###if (featurep! +lsp)

(def-package! lsp-java
  :after-call java-mode
  :init (add-hook 'java-mode-hook #'lsp!)
  :config
  ;; TODO keybinds
  ;; TODO treemacs integration (?)
  )
