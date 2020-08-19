;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "6b4377686128d5c2fb55d8fe61b92a9991d40fbd")

(when (featurep! :completion company)
  (package! company-lean :pin "6b4377686128d5c2fb55d8fe61b92a9991d40fbd"))
