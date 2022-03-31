;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "c1c68cc946eb31b6ba8faefdceffce1f77ca52df")

(when (featurep! :completion company)
  (package! company-lean :pin "c1c68cc946eb31b6ba8faefdceffce1f77ca52df"))
