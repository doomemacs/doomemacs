;;; lang/beancount/config.el -*- lexical-binding: t; -*-

(use-package! beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :hook (beancount-mode . outline-minor-mode)
  :init
  ;; REVIEW Remove once domtronn/all-the-icons.el#272 is merged
  (after! all-the-icons
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.beancount\\'" all-the-icons-material "attach_money" :face all-the-icons-lblue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(beancount-mode all-the-icons-material "attach_money" :face all-the-icons-lblue)))

  :config
  (setq beancount-electric-currency t)

  (when (featurep! +lsp)
    (add-hook 'beancount-mode-local-vars-hook #'lsp! 'append))

  (map! :map beancount-mode-map
        "TAB" (cmds! (and outline-minor-mode (outline-on-heading-p))
                     #'beancount-outline-cycle
                     #'indent-according-to-mode)
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
