;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "e1dc7caee76d117a366f8b8b1c2da7e6400636a8")
(package! cider :pin "d8fd5794c909e671641f534b1eb21db0e92138e9")
(package! clj-refactor :pin "466822ff6f9da584f7cf72c868017b8840574dbd")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "a558bda44c4cb65b69fa53df233e8941ebd195c5"))
