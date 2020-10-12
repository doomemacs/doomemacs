;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "e72677668f5fc7cc148008e885a0f256e245dd43")

(when (featurep! +dante)
  (package! dante :pin "e2acbf6dd37818cbf479c9c3503d8a59192e34af")
  (package! attrap :pin "9c881548debcf59b8aadda0ef4abca3c9a68dd80"))
(when (or (and (featurep! +lsp)
               (not (featurep! :tools lsp +eglot)))
          (featurep! +ghcide))
  (package! lsp-haskell :pin "4d85cb3693d893ec34d8a0be9794d468a0a28b7b"))
