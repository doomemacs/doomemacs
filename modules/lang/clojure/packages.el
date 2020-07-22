;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "da9f1ec717dac1194404b4a4562dba6bd9a4ee3a")
(package! cider :pin "3d8552a8469a5515cd6e783b5a9ab09ba9d51eb5")
(package! clj-refactor :pin "97095682580bbc5bfebcbc5349f03f5bd7121c96")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "5472c26ffdf754a0661357564874ffd4f8598805"))
