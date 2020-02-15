;;; lang/dart/+flutter.el -*- lexical-binding: t; -*-

(use-package! flutter
  :config
  (map! :map dart-mode-map
        :localleader
        "r" #'flutter-run-or-hot-reload))
