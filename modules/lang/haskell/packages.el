;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode :pin "41683c0e634bb3f54eac8747919a82132e1714fe")

(when (featurep! +dante)
  (package! dante :pin "c516bc9e8f09e0f928de9a93e82acfb382636f5c")
  (package! attrap :pin "4cf3e4a16255997e7c3c39682a72866a0a37dd4b"))
(when (or (and (featurep! +lsp)
               (not (featurep! :tools lsp +eglot)))
          (featurep! +ghcide))
  (package! lsp-haskell :pin "17d7d4c6615b5e6c7442828720730bfeda644af8"))
