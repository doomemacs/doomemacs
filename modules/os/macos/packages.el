;; -*- no-byte-compile: t; -*-
;;; os/macos/packages.el

(when (< emacs-major-version 29)
  (package! osx-trash :pin "90f0c99206022fec646206018fcd63d9d2e57325"))
(package! ns-auto-titlebar :pin "60273e764bf8d95abc40dd2fdc23af87ea9ee33b")
