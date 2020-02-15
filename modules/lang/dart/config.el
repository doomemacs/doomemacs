;;; lang/dart/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +flutter) (load! "+flutter"))
      ((featurep! +lsp)    (load! "+lsp")))

(use-package! dart-mode
  :config
  (when (featurep! +flutter)
    (if IS-LINUX
      (setq lsp-dart-sdk-dir "/opt/flutter/bin/cache/dart-sdk/")))
  (when (featurep! +lsp)
    (add-hook 'dart-mode-hook 'lsp)))
