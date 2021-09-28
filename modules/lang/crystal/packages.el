;; -*- no-byte-compile: t; -*-
;;; lang/crystal/packages.el

(package! crystal-mode :pin "15998140b0a4172cd4b4d14d0377fba96a8917fc")
(package! inf-crystal :pin "02007b2a2a3bea44902d7c83c4acba1e39d278e3")
(when (featurep! :checkers syntax)
  (package! flycheck-crystal :pin "15998140b0a4172cd4b4d14d0377fba96a8917fc")
  (package! flycheck-ameba :pin "0c4925ae0e998818326adcb47ed27ddf9761c7dc"))
