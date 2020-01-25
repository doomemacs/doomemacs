;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! cider :pin "7437c67f0e")
(package! clj-refactor :pin "e24ba62843")

(when (featurep! :checkers syntax)
  (package! flycheck-joker :pin "51e99e6977"))
