;; -*- no-byte-compile: t; -*-
;;; lang/crystal/packages.el

(package! crystal-mode :pin "2428b01624")
(package! inf-crystal :pin "02007b2a2a")
(when (featurep! :checkers syntax)
  (package! flycheck-crystal :pin "2428b01624")
  (package! flycheck-ameba :pin "0c4925ae0e"))
