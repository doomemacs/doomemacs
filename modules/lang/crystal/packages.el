;; -*- no-byte-compile: t; -*-
;;; lang/crystal/packages.el

(package! crystal-mode :pin "96a8058205b24b513d0b9307db32f05e30f9570b")
(package! inf-crystal :pin "02007b2a2a3bea44902d7c83c4acba1e39d278e3")
(when (featurep! :checkers syntax)
  (package! flycheck-crystal :pin "96a8058205b24b513d0b9307db32f05e30f9570b")
  (package! flycheck-ameba :pin "0c4925ae0e998818326adcb47ed27ddf9761c7dc"))
