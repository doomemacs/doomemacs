;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "8d0f44bfe2a9ab6b0969c9bafb75089f315ff5ae")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "918ffa2516a59c90f909b584f7c9968716c0e006"))
