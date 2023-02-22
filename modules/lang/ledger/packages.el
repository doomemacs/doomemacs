;; -*- no-byte-compile: t; -*-
;;; lang/ledger/packages.el

(package! ledger-mode :pin "4b32f701736b37f99048be79583b0bde7cc14c85")

(when (modulep! :editor evil)
  (package! evil-ledger :pin "7a9f9f5d39c42fffdba8004f8982642351f2b233"))

(when (modulep! :checkers syntax)
  (package! flycheck-ledger :pin "628e25ba66604946085571652a94a54f4d1ad96f"))
