;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (featurep! :completion company)
  (package! company-shell :pin "52f3bf26b74adc30a275f5f4290a1fc72a6876ff"))

(when (featurep! +fish)
  (package! fish-mode :pin "db257db81058b0b12f788c324c264cc59b9a5bf4"))

(when (featurep! +powershell)
  (package! powershell :pin "d1b3f95669343399f199f291ef76c09a0ede5e60"))
