;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "53ef8ac076ae7811627fbdd408e519ab7fca9a0b")
(package! cider :pin "ef47c1de151c212b8d2ddeb9af6c8b0dfc0e300f")
(package! clj-refactor :pin "6db85b37b57497b56d97d5e5512160e5db85f798")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "a558bda44c4cb65b69fa53df233e8941ebd195c5"))
