;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "e72677668f5fc7cc148008e885a0f256e245dd43")

(when (featurep! +dante)
  (package! dante :pin "e2acbf6dd37818cbf479c9c3503d8a59192e34af")
  (package! attrap :pin "9c881548debcf59b8aadda0ef4abca3c9a68dd80"))
(when (or (and (featurep! +lsp)
               (not (featurep! :tools lsp +eglot)))
          (featurep! +ghcide))
  (package! lsp-haskell :pin "a56667b496e5370f1a50310589a2d2a4d3b9d11e"))
