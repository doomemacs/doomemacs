;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "0d39c847fddddc5b76fe3c706e34ab45439760bc")

(when (featurep! +dante)
  (package! dante :pin "e2acbf6dd37818cbf479c9c3503d8a59192e34af")
  (package! attrap :pin "9c881548debcf59b8aadda0ef4abca3c9a68dd80"))
(when (or (and (featurep! +lsp)
               (not (featurep! :tools lsp +eglot)))
          (featurep! +ghcide))
  (package! lsp-haskell :pin "5d3f4814f6ac44547a62551472cc76fbaebcccf7"))
