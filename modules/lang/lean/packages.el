;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "f26e40daad2c1bd090e440a2b931205ac3b9b613")

(when (featurep! :completion company)
  (package! company-lean :pin "f26e40daad2c1bd090e440a2b931205ac3b9b613"))
