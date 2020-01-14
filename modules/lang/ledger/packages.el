;; -*- no-byte-compile: t; -*-
;;; lang/ledger/packages.el

(package! ledger-mode)

(when (featurep! :editor evil)
  (package! evil-ledger))

(when (featurep! :checkers syntax)
  (package! flycheck-ledger))
