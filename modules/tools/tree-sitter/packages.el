;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :recipe (:branch "master") :pin "12f92ef3992c1d317574e968707a9550bd1fc89f")
(package! tree-sitter-langs :pin "4eb4989d1a708e81639a7ef7dbff43a34894caee")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "bce236e5d2cc2fa4eae7d284ffd19ad18d46349a"))
