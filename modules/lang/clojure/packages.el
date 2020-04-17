;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "c970c4605c")
(package! cider :pin "d63e5652fd")
(package! clj-refactor :pin "8259791e05")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "5472c26ffd"))
