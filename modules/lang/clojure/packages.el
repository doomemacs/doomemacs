;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el

(package! cider :pin "52dcc60cd5")
(package! clj-refactor :pin "92d372393a")

(when (featurep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "f652a8dc4c"))
