;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "cb573c8db5b856eb37473009f2c62e0717a1cd02")

(when (and (featurep! +lsp)
           (not (featurep! :tools lsp +eglot)))
  (package! lsp-haskell :pin "7cf64944ab3a25ea5d6f8d5e0cd33124182df991"))
