;;; lang/ledger/config.el -*- lexical-binding: t; -*-

;; `ledger-mode'
(setq ledger-clear-whole-transactions 1)


(def-package! evil-ledger
  :when (featurep! :feature evil)
  :hook (ledger-mode . evil-ledger-mode))


(def-package! flycheck-ledger
  :when (featurep! :feature syntax-checker)
  :after ledger-mode)
