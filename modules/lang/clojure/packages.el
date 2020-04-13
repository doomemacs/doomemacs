;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "c970c4605c")
(package! cider :pin "8f5657bc35")
(package! clj-refactor :pin "8259791e05")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "d46745c416"))
