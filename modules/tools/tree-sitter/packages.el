;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "771239bacecf6c3ba9ee8b9eecec2b9fdd8e2256")

(package! tree-sitter-langs
  :pin "a9b0390a751be0a631cf8a356d61933795d9fcbc")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "607b71f38a1b2d7fa464814d968427435d31dd7c"))
