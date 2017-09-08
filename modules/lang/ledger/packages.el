;; -*- no-byte-compile: t; -*-
;;; lang/ledger/packages.el

(package! ledger-mode)

(when (featurep! :feature evil)
  (package! evil-ledger))

(when (featurep! :feature syntax-checker)
  (package! flycheck-ledger))
