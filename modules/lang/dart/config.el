;;; lang/dart/config.el -*- lexical-binding: t; -*-

(use-package! dart-mode
  :when (featurep! +lsp)
  :hook (dart-mode-local-vars . lsp!)
  :config
  (when (and (featurep! +flutter) IS-LINUX)
    (when-let (path (doom-glob "/opt/flutter/bin/cache/dart-sdk"))
      (setq flutter-sdk-path path))))


(use-package! flutter
  :when (featurep! +flutter)
  :defer t
  :config
  (map! :map dart-mode-map
        :localleader
        "r" #'flutter-run-or-hot-reload))


(use-package! hover
  :when (featurep! +flutter)
  :defer t
  :config
  (map! :map dart-mode-map
        :localleader
        "h r" #'hover-run-or-hot-reload
        "h R" #'hover-run-or-hot-restart))
