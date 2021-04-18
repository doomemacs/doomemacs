;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "5c50338ac149ca5225fc737be291db1f63c45f1d")

(when (featurep! :completion company)
  (package! company-lean :pin "5c50338ac149ca5225fc737be291db1f63c45f1d"))
