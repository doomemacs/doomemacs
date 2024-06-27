;; -*- no-byte-compile: t; -*-
;;; lang/ledger/packages.el

(package! ledger-mode :pin "b0e65f74a5a0db98fd1aea2bd87fcf9e489cdacb")

(when (modulep! :editor evil)
  (package! evil-ledger :pin "7a9f9f5d39c42fffdba8004f8982642351f2b233"))

(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-ledger :pin "628e25ba66604946085571652a94a54f4d1ad96f"))
