;; -*- no-byte-compile: t; -*-
;;; lang/hledger/packages.el

(package! hledger-mode :pin "5492509a23047f0a1f05a112b47fa34eba7c5e1d")

(when (modulep! :editor evil)
  (package! evil-ledger :pin "7a9f9f5d39c42fffdba8004f8982642351f2b233"))
