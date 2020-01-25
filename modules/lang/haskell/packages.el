;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "3cf99d7f0e")

(when (featurep! +dante)
  (package! dante :pin "3e532e8d7e")
  (package! attrap :pin "4cf3e4a162"))
(when (featurep! +lsp)
  (package! lsp-haskell :pin "6d481f97e6"))
;; DEPRECATED
(when (featurep! +intero)
  (package! intero :pin "30d8e7330c"))
