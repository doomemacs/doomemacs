;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "5e1091658d625984c6c5756e3550c4d2eebd73a1")

(package! tree-sitter-langs
  :pin "f4effc81fcac3592bce7072619a0e17043412cf4")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "f3b3e9554e5ecae55200454804e183e268b4a6fc"))
