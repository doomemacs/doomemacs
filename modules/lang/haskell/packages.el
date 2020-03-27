;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "7032966ee7")

(when (featurep! +dante)
  (package! dante :pin "4955bc7363")
  (package! attrap :pin "4cf3e4a162"))
(when (featurep! +lsp)
  (package! lsp-haskell :pin "582fa27c88"))
;; DEPRECATED
(when (featurep! +intero)
  (package! intero :pin "fdb0550a2d"))
