;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "bf32bb97930ed67c5cbe0fe3d4a69dedcf68be44")

(when (featurep! :completion company)
  (package! company-lean :pin "bf32bb97930ed67c5cbe0fe3d4a69dedcf68be44"))
