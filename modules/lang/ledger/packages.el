;; -*- no-byte-compile: t; -*-
;;; lang/ledger/packages.el

(package! ledger-mode :pin "a514953d6a")

(when (featurep! :editor evil)
  (package! evil-ledger :pin "7a9f9f5d39"))

(when (featurep! :checkers syntax)
  (package! flycheck-ledger :pin "2065beab56"))
