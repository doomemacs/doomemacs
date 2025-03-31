;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "be2639592f0fd3c779bcdcec54e2124277baa03f")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-haskell :pin "b2edf1a9f8ae97d6fdc566de7706e8c9c0a26606"))
