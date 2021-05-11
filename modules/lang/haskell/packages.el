;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "1baa12abfa2c81128e5b13d1351f2978a4a54b4f")

(when (featurep! +dante)
  (package! dante :pin "8741419333fb85ed2c1d71f5902688f5201b0a40")
  (package! attrap :pin "778382eba8e1a449862b1573e90c1e79cf5caeb1"))
(when (or (and (featurep! +lsp)
               (not (featurep! :tools lsp +eglot)))
          (featurep! +ghcide))
  (package! lsp-haskell :pin "7efbef3d206989faa8b691a4230a3ed872542187"))
