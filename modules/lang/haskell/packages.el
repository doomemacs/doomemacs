;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "be2639592f0fd3c779bcdcec54e2124277baa03f")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-haskell :pin "cd0f5d251c14e90f2896d26d18de8ace462e011b"))
