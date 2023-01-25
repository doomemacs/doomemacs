;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "90503413f4cdb0ed26871e39c4e6e2552b57f7db")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "3249cde75fb411f95fe173c222b848182fd0b752"))
