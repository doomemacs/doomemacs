;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "98ba3922360199d5260d47f417f096730ad057c5")

(when (featurep! +dante)
  (package! dante :pin "8741419333fb85ed2c1d71f5902688f5201b0a40")
  (package! attrap :pin "a5bc695af27349ae6fe4541a581e6fd449d2a026"))
(when (or (and (featurep! +lsp)
               (not (featurep! :tools lsp +eglot)))
          (featurep! +ghcide))
  (package! lsp-haskell :pin "eb37ac4a6a43277263bbb17aed6a862a0992ae8e"))
