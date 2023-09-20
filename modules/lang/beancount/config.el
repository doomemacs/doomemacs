;;; lang/beancount/config.el -*- lexical-binding: t; -*-

(use-package! beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :hook (beancount-mode . outline-minor-mode)
  :init
  (after! nerd-icons
    (add-to-list 'nerd-icons-extension-icon-alist
                 '("beancount" nerd-icons-faicon "nf-fa-money" :face nerd-icons-lblue))
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(beancount-mode nerd-icons-faicon "nf-fa-money" :face nerd-icons-lblue)))

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
        "s" #'+beancount/occur
        "l" #'beancount-linked
        "q" #'beancount-query
        "x" #'beancount-context
        (:prefix ("i" . "insert")
         "c" #'+beancount/clone-transaction
         "C" #'+beancount/clone-this-transaction
         "a" #'beancount-insert-account
         "p" #'beancount-insert-prices
         "d" #'beancount-insert-date)))
