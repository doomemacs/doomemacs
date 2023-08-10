;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "79af559b6d750aff32a916a81cc4fb63ecfd588d")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "3249cde75fb411f95fe173c222b848182fd0b752"))
