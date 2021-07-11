;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "3e426b3a479f479963f2c7d1147cc826ed1a0ee1")
(package! cider :pin "fe8cf244fd3426261f9f630c981a6296afd433a4")
(package! clj-refactor :pin "466822ff6f9da584f7cf72c868017b8840574dbd")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "a558bda44c4cb65b69fa53df233e8941ebd195c5"))
