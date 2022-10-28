;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "90503413f4cdb0ed26871e39c4e6e2552b57f7db")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "485c1148ce4d27030bb95b21c7289809294e7d31"))
