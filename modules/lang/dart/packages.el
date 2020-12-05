;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "04fcd649f19d49390079fbf2920a10bf37f6a634")

(when (featurep! +lsp)
  (package! lsp-dart :pin "b81b1eced51da99647830b829346ba2c4e1b23e0"))

(when (featurep! +flutter)
  (package! flutter :pin "696228a619f6078b16f9f77071112f6ad2a25c4e")
  (package! hover :pin "3f07a181ec38531e01692e4073d6968363697cf8"))
