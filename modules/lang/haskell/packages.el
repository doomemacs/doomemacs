;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "727f72a2a4b8e4fd0a7b62129668baea55a2c3e0")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-haskell :pin "ba49fa9822556aff58aa47929cd426e9427baaea"))
