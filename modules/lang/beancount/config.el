;;; lang/beancount/config.el -*- lexical-binding: t; -*-

(use-package! beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :hook (beancount-mode . outline-minor-mode)
  :config
  (setq beancount-electric-currency t)

  (when (modulep! +lsp)
    (add-hook 'beancount-mode-local-vars-hook #'lsp! 'append))

  (map! :map beancount-mode-map
        "TAB" (cmds! (and outline-minor-mode (outline-on-heading-p))
                     #'beancount-outline-cycle
                     #'indent-according-to-mode)
        :m "[[" #'+beancount/previous-transaction
        :m "]]" #'+beancount/next-transaction
        :localleader
        "b" #'+beancount/balance
        "c" #'beancount-check
        "l" #'beancount-linked
        "q" #'beancount-query
        "x" #'beancount-context
        (:prefix ("i" . "insert")
         "c" #'+beancount/clone-transaction
         "C" #'+beancount/clone-this-transaction
         "a" #'beancount-insert-account
         "p" #'beancount-insert-prices
         "d" #'beancount-insert-date)))
