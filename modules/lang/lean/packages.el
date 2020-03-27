;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "65b55b1711")

(when (featurep! :completion company)
  (package! company-lean :pin "65b55b1711"))
