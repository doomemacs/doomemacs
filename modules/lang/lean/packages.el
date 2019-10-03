;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode)

(when (featurep! :completion company)
  (package! company-lean))
