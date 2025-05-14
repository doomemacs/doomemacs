;;; lang/beancount/config.el -*- lexical-binding: t; -*-

(use-package! beancount
  :hook (beancount-mode . outline-minor-mode)
  :hook (beancount-mode . flymake-bean-check-enable) ; FIXME: add proper flycheck support
  :init
  (after! nerd-icons
    (add-to-list 'nerd-icons-extension-icon-alist
                 '("beancount" nerd-icons-faicon "nf-fa-money" :face nerd-icons-lblue))
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(beancount-mode nerd-icons-faicon "nf-fa-money" :face nerd-icons-lblue)))

  :config
  (set-eval-handler! 'beancount-mode #'beancount-region-default)

  (setq beancount-electric-currency t)

  (when (modulep! +lsp)
    (add-hook 'beancount-mode-local-vars-hook #'lsp! 'append))

  ;; HACK: The intro message could contain ANSI color codes, causing the regexp
  ;;   in `beancount--fava-filter' to fail to match it (and thus the browser
  ;;   isn't automatically opened after executing `beancount-fava').
  ;; REVIEW: PR this upstream!
  (defadvice! +beancount--open-in-browser-after-starting-fix-a (fn process output)
    :around #'beancount--fava-filter
    (funcall fn process (ansi-color-filter-apply output)))

  (map! :map beancount-mode-map
        :m "[[" #'+beancount/previous-transaction
        :m "]]" #'+beancount/next-transaction
        :localleader
        "b" #'+beancount/balance
        "c" #'beancount-check
        "S" #'+beancount/occur
        "l" #'beancount-linked
        "q" #'beancount-query
        "x" #'beancount-context
        (:prefix ("i" . "insert")
         "c" #'+beancount/clone-transaction
         "C" #'+beancount/clone-this-transaction
         "a" #'beancount-insert-account
         "p" #'beancount-insert-prices
         "d" #'beancount-insert-date)
        (:prefix ("s" . "sort")
         "r" #'+beancount/sort-region
         "b" #'+beancount/sort-buffer)))
