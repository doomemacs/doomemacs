;; -*- no-byte-compile: t; -*-
;;; lang/crystal/packages.el

(package! crystal-mode :pin "ea89b108fa4222df94ffb99e6e7eaec5d7aa4fea")
(package! inf-crystal :pin "02007b2a2a3bea44902d7c83c4acba1e39d278e3")
(when (modulep! :checkers syntax -flymake)
  (package! flycheck-crystal :pin "ea89b108fa4222df94ffb99e6e7eaec5d7aa4fea")
  (package! flycheck-ameba :pin "0c4925ae0e998818326adcb47ed27ddf9761c7dc"))
