;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter :pin "3cfab8a0e945db9b3df84437f27945746a43cc71")
(package! tree-sitter-langs :pin "d8f8ac4faeb4564fbb61e94a631b4672523d84a9")
(package! tree-sitter-indent :pin "4ef246db3e4ff99f672fe5e4b416c890f885c09e")

(when (modulep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "02f8253034042d8f171bc0ef93e3538b71a29153"))
