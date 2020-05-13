;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "da9f1ec717dac1194404b4a4562dba6bd9a4ee3a")
(package! cider :pin "3a59fe046c9884573512dff88d140d37c0be7b2c")
(package! clj-refactor :pin "8259791e054382457b87d1f78061b5e3ce948907")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "5472c26ffdf754a0661357564874ffd4f8598805"))
