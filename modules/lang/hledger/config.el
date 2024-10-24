;;; lang/hledger/config.el -*- lexical-binding: t; -*-

(use-package! hledger-mode
  :defer t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :init
  (set-company-backend! 'hledger-mode 'hledger-company)
  :config
  (map! :map hledger-mode-map
        :localleader
        ("r" #'hledger-reschedule
         "e" #'hledger-edit-amount
         "s" #'hledger-toggle-star
         "a" #'hledger-add-days-to-entry-date
         "j" #'hledger-forward-entry
         "k" #'hledger-backward-entry
         "y" #'hledger-copy-to-clipboard
         "i" #'hledger-increment-entry-date
         "d" #'hledger-decrement-entry-date
         "n" #'hledger-jentry)))

(use-package! evil-ledger
  :when (modulep! :editor evil +everywhere)
  :hook (hledger-mode . evil-ledger-mode))
