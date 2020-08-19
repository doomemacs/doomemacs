;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! clojure-mode :pin "84ed16c5ddb6561620886485e20669d0c81f88a1")
(package! cider :pin "a89b694cc3cec0294d84bf9dbe1163ad2373e8db")
(package! clj-refactor :pin "97095682580bbc5bfebcbc5349f03f5bd7121c96")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "5472c26ffdf754a0661357564874ffd4f8598805"))
