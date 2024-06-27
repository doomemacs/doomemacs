;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "727f72a2a4b8e4fd0a7b62129668baea55a2c3e0")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "18a7c7881fb249d9b4cb5f376dfa84682022dc83"))
