;;; lang/beancount/config.el -*- lexical-binding: t; -*-

(use-package! beancount-mode
  :mode "\\.beancount\\'"
  :init
  (add-hook 'beancount-mode-hook #'outline-minor-mode)

  (after! all-the-icons
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.beancount\\'" all-the-icons-material "attach_money" :face all-the-icons-lblue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(beancount-mode all-the-icons-material "attach_money" :face all-the-icons-lblue)))
  :config
  (when (featurep! +lsp)
    (add-hook 'beancount-mode-local-vars-hook #'lsp!))

  (setq beancount-electric-currency t)

  (map! :map beancount-mode-map
        :localleader
        "b" #'+beancount/balance
        "c" #'beancount-check
        "l" #'beancount-linked
        "q" #'beancount-query
        "x" #'beancount-context
        (:prefix ("i" . "insert")
         "a" #'beancount-insert-account
         "p" #'beancount-insert-prices
         "d" #'beancount-insert-date)))
