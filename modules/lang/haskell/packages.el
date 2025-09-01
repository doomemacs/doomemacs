;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "e5d32021ea30438fb957976760b94af66a55b53b")
(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! haskell-ts-mode :pin "b47211699944997bfb03fd88b1157dd71727bad7"))

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-haskell :pin "aa9b5bce355790de1fbdbe239650d704f46a19a0"))
