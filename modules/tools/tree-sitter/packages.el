;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")

(package! tree-sitter-langs
  :pin "0dd5e56e2f5646aa51ed0fc9eb869a8f7090228a")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "bfdef5a292f7dde36967bb86eb2f7009b03631b1"))
