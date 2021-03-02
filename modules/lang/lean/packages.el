;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "15bee87dc4080b87c543964375b7ce162317ab6f")

(when (featurep! :completion company)
  (package! company-lean :pin "15bee87dc4080b87c543964375b7ce162317ab6f"))
