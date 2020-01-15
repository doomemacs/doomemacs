;; -*- no-byte-compile: t; -*-
;;; lang/crystal/packages.el

(package! crystal-mode :pin "2428b016243e78a0312cf6b3ba6939e7169a1405")
(package! inf-crystal :pin "02007b2a2a3bea44902d7c83c4acba1e39d278e3")
(when (featurep! :checkers syntax)
  (package! flycheck-crystal :pin "2428b016243e78a0312cf6b3ba6939e7169a1405")
  (package! flycheck-ameba :pin "0c4925ae0e998818326adcb47ed27ddf9761c7dc"))
