;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "5e1091658d625984c6c5756e3550c4d2eebd73a1")

(package! tree-sitter-langs
  :pin "0dd5e56e2f5646aa51ed0fc9eb869a8f7090228a")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "e5fda8eca926e65f7aadc9ed27d768eb6d1d415f"))
