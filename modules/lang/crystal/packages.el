;; -*- no-byte-compile: t; -*-
;;; lang/crystal/packages.el

(package! crystal-mode :pin "d913fea6f0968e0e258d0baf10ded8db9952de52")
(package! inf-crystal :pin "02007b2a2a3bea44902d7c83c4acba1e39d278e3")
(when (modulep! :checkers syntax -flymake)
  (package! flycheck-crystal :pin "d913fea6f0968e0e258d0baf10ded8db9952de52")
  (package! flycheck-ameba :pin "0c4925ae0e998818326adcb47ed27ddf9761c7dc"))
