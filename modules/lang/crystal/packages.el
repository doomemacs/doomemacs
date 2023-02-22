;; -*- no-byte-compile: t; -*-
;;; lang/crystal/packages.el

(package! crystal-mode :pin "9bfb9f0f566e937cc6a2f2913d1b56978b81dc99")
(package! inf-crystal :pin "02007b2a2a3bea44902d7c83c4acba1e39d278e3")
(when (modulep! :checkers syntax)
  (package! flycheck-crystal :pin "9bfb9f0f566e937cc6a2f2913d1b56978b81dc99")
  (package! flycheck-ameba :pin "0c4925ae0e998818326adcb47ed27ddf9761c7dc"))
