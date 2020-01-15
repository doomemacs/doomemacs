;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "1ac4de398f9e19f322fa168ebf58527458e43df5")

(when (featurep! +dante)
  (package! dante :pin "3e532e8d7ea02d6045345d1175e05b616882112a")
  (package! attrap :pin "4cf3e4a16255997e7c3c39682a72866a0a37dd4b"))
(when (featurep! +lsp)
  (package! lsp-haskell :pin "6d481f97e62b0fd2455e8f7a36429981277445b1"))
;; DEPRECATED
(when (featurep! +intero)
  (package! intero :pin "30d8e7330c9b20c2905035bc417fa8645e7b4b85"))
