;; -*- no-byte-compile: t; -*-
;;; lang/ledger/packages.el

(package! ledger-mode :pin "a514953d6a25cb29c0ec218e9824ee201c9b904d")

(when (featurep! :editor evil)
  (package! evil-ledger :pin "7a9f9f5d39c42fffdba8004f8982642351f2b233"))

(when (featurep! :checkers syntax)
  (package! flycheck-ledger :pin "2065beab564c23e6ab380547d19bdb5a9b3b25fc"))
