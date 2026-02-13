;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "2dd755a5fa11577a9388af88f385d2a8e18f7a8d")
(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! haskell-ts-mode :pin "bf143ee8382f09e0a68d775d80445065f32929c3"))

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-haskell :pin "871a0ef2e98b3a749d0b69d958698000ca5640d3"))
