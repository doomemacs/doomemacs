;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "b5c913af12ab3f55723fa3f57d373a4984655e8a ")
(package! cider :pin "815204f65db8b28524e8eab6421e6d8813a87e89")
(package! clj-refactor :pin "b24ce76acefe792975f00147c94b4dd784e65b80")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "a558bda44c4cb65b69fa53df233e8941ebd195c5"))
