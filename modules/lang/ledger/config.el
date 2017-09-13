;;; lang/ledger/config.el -*- lexical-binding: t; -*-

(def-package! ledger-mode
  :mode "\\.ledger$"
  :commands ledger-mode
  :config
  (setq ledger-clear-whole-transactions 1))


(def-package! evil-ledger
  :when (featurep! :feature evil)
  :after ledger-mode
  :config
  (add-hook 'ledger-mode-hook #'evil-ledger-mode))


(def-package! flycheck-ledger
  :when (featurep! :feature syntax-checker)
  :init (add-hook 'ledger-mode-hook #'flycheck-mode))
