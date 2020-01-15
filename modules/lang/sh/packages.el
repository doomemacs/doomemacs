;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (featurep! :completion company)
  (package! company-shell :pin "52f3bf26b74adc30a275f5f4290a1fc72a6876ff"))

(when (featurep! +fish)
  (package! fish-mode :pin "688c82decad108029b0434e3bce6c3d129ede6f3"))
