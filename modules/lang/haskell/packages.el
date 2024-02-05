;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "43b4036bf02b02de75643a1a2a31e28efac1c50b")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "89d16370434e9a247e95b8b701f524f5abfc884b"))
