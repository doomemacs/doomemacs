;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "1a285fc4c50ca74bb5cd9b2a8c1a46a64a77384a")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-haskell :pin "6981f8d1225c038c1a130e8cf70530cfe15f976e"))
