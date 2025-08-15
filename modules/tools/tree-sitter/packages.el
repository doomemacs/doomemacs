;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :recipe (:branch "master") :pin "caeb32e8a7783a8a6c14ac7b2d84d415e8a582ff")
(package! tree-sitter-langs :pin "9b7e5084353b89d28a5ed33503731c86b4d6ec70")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "4ca5dffbd3f81361c85203bde44328ad2128d33a"))
