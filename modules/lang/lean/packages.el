;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "f26e40daad")

(when (featurep! :completion company)
  (package! company-lean :pin "f26e40daad"))
