;; -*- no-byte-compile: t; -*-
;;; lang/ledger/packages.el

(package! ledger-mode :pin "805507fd6c14839be4efc7aee2017f9c03e36834")

(when (featurep! :editor evil)
  (package! evil-ledger :pin "7a9f9f5d39c42fffdba8004f8982642351f2b233"))

(when (featurep! :checkers syntax)
  (package! flycheck-ledger :pin "628e25ba66604946085571652a94a54f4d1ad96f"))
