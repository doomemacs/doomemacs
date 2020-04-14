;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "7032966ee7")

(when (featurep! +dante)
  (package! dante :pin "4955bc7363")
  (package! attrap :pin "4cf3e4a162"))
(when (or (featurep! +lsp)
          (featurep! +ghcide))
  (package! lsp-haskell :pin "582fa27c88"))
