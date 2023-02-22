;; -*- no-byte-compile: t; -*-
;;; lang/lean/packages.el

(package! lean-mode :pin "362bc6fa3efb1874c525ed6b4b6f24f76af22596")

(when (modulep! :completion company)
  (package! company-lean :pin "362bc6fa3efb1874c525ed6b4b6f24f76af22596"))
