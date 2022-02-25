;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "5e1091658d625984c6c5756e3550c4d2eebd73a1")

(package! tree-sitter-langs
  :pin "599570cd2a6d1b43a109634896b5c52121e155e3")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "ff733576d1dc5395c08d8f0e396b7a7073e39674"))
