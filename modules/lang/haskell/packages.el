;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "7f452cc9e6c3316b5a4a2b790d3a396f271609d9")

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! lsp-haskell :pin "b2edf1a9f8ae97d6fdc566de7706e8c9c0a26606"))
