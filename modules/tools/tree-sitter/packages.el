;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  ;; :ignore (null (bound-and-true-p module-file-suffix))
  :pin "c7a1c34549cad41a3618c6f17e0e9dabd3e98fe1")
(package! tree-sitter-langs
  ;; :ignore (null (bound-and-true-p module-file-suffix))
  :pin "e7b8db7c4006c04a4bc1fc6865ec31f223843192")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "f20598676f99e6fa33759d9807e94f42271c0dfb"))
