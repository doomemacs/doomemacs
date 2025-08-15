;; -*- no-byte-compile: t; -*-
;;; os/macos/packages.el

(when (< emacs-major-version 29)
  (package! osx-trash :pin "90f0c99206022fec646206018fcd63d9d2e57325"))
(package! ns-auto-titlebar :pin "1205ac67b76b58e9eb53d2115b85775533653a80")
