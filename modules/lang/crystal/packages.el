;; -*- no-byte-compile: t; -*-
;;; lang/crystal/packages.el

(package! crystal-mode :pin "f9e4db16ff9fdc6a296363aa35d19cfb4926e472")
(package! inf-crystal :pin "02007b2a2a3bea44902d7c83c4acba1e39d278e3")
(when (featurep! :checkers syntax)
  (package! flycheck-crystal :pin "f9e4db16ff9fdc6a296363aa35d19cfb4926e472")
  (package! flycheck-ameba :pin "0c4925ae0e998818326adcb47ed27ddf9761c7dc"))
