;; -*- no-byte-compile: t; -*-
;;; lang/ledger/packages.el

(package! ledger-mode :pin "9ab399186fad220f59f3c1bbbcaddabf49ed9de8")

(when (modulep! :editor evil)
  (package! evil-ledger :pin "7a9f9f5d39c42fffdba8004f8982642351f2b233"))

(when (modulep! :checkers syntax -flymake)
  (package! flycheck-ledger :pin "48bed9193c8601b142245df03968ae493b7d430c"))
