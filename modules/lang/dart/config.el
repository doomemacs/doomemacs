;;; lang/dart/config.el -*- lexical-binding: t; -*-

(use-package! dart-mode
  :defer t
  :config
  (when (and (featurep! +flutter) IS-LINUX)
    (setq lsp-dart-sdk-dir "/opt/flutter/bin/cache/dart-sdk/"))
  (when (featurep! +lsp)
    (add-hook 'dart-mode-local-vars-hook #'lsp!)))


(when (featurep! +flutter)
  (use-package! flutter
    :defer t
    :config
    (map! :map dart-mode-map
          :localleader
          "r" #'flutter-run-or-hot-reload)))
