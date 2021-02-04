;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "53ef8ac076ae7811627fbdd408e519ab7fca9a0b")
(package! cider :pin "1a34f893e1fe81982fb5099192122a72ee8e94ea")
(package! clj-refactor :pin "9dcc50da7ce6f3c10276c87f09022e80c03e8bef")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "a558bda44c4cb65b69fa53df233e8941ebd195c5"))
