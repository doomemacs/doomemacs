;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "a34ccdc54be15043ff0d253c3c20087524255491")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "3249cde75fb411f95fe173c222b848182fd0b752"))
