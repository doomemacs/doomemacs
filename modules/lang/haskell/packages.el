;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "383b4b77753ef83420c7a755f86e1ec4770f551b")
(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! haskell-ts-mode :pin "bf143ee8382f09e0a68d775d80445065f32929c3"))

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-haskell :pin "871a0ef2e98b3a749d0b69d958698000ca5640d3"))
