;; -*- no-byte-compile: t; -*-
;;; lang/ledger/packages.el

(package! ledger-mode :pin "a6be7a2d79281a442ce5abd3f073b6c20d7d11d8")

(when (modulep! :editor evil)
  (package! evil-ledger :pin "7a9f9f5d39c42fffdba8004f8982642351f2b233"))

(when (modulep! :checkers syntax -flymake)
  (package! flycheck-ledger :pin "628e25ba66604946085571652a94a54f4d1ad96f"))
