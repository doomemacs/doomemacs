;;; lang/beancount/config.el -*- lexical-binding: t; -*-

(defvar +beancount-files 'auto
  "A list of beancount files to factor into completion & linting.

Order is important!

Can also be set to `auto' to automatically (and recursively) crawl include
statements to build this file list dynamically (which is cached on a per-buffer
basis). The first time this happens it can be very slow in large file
hierarchies or with massive beancount files.

If set to `nil', only the current buffer is considered (the original
behavior).")
(put '+beancount-files 'safe-local-variable #'stringp)


;;
;;; Packages

(use-package! beancount
  :mode ("\\.bean\\'" . beancount-mode)
  :hook (beancount-mode . outline-minor-mode)
  :init
  (after! nerd-icons
    (add-to-list 'nerd-icons-extension-icon-alist
                 '("beancount" nerd-icons-faicon "nf-fa-money" :face nerd-icons-lblue))
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(beancount-mode nerd-icons-faicon "nf-fa-money" :face nerd-icons-lblue)))
  :config
  (set-eval-handler! 'beancount-mode #'beancount-region-default)

  (setq beancount-electric-currency t)

  ;; Fontify custom directives.
  ;; REVIEW: PR this upstream.
  (add-to-list 'beancount-font-lock-keywords
               `(,(concat "^\\(" beancount-date-regexp "\\) +"
                          "\\(" (regexp-opt '("custom")) "\\) +")
                 (1 'beancount-date)
                 (2 'beancount-directive)))

  (add-hook 'beancount-mode-local-vars-hook
            (if (modulep! +lsp)
                #'lsp!
              #'flymake-bean-check-enable)  ; FIXME: add proper flycheck support
            'append)

  ;; HACK: The intro message could contain ANSI color codes, causing the regexp
  ;;   in `beancount--fava-filter' to fail to match it (and thus the browser
  ;;   isn't automatically opened after executing `beancount-fava').
  ;; REVIEW: PR this upstream!
  (defadvice! +beancount--open-in-browser-after-starting-fix-a (fn process output)
    :around #'beancount--fava-filter
    (funcall fn process (ansi-color-filter-apply output)))

  ;; HACK: This makes a couple adjustments to beancount-mode's flymake linter:
  ;;
  ;;   1. Widens the buffer so bean-check can see the full buffer and won't
  ;;      complain about missing context.
  ;;   2. Replaces any relative file paths in include and document directives
  ;;      with an absolute path, so bean-check doesn't throw false positives
  ;;      about missing files relative to /dev (because flymake-bean is piping
  ;;      context to /dev/stdin).
  ;;   3. Adds support for meta lines that only the flymake linter will see.
  ;;      These are lines prefixed by any number of semicolons followed by a hash
  ;;      then space. E.g.
  ;;
  ;;      ;# include "../config.beancount"
  ;;      ;# 2025-01-01 pad Assets:Bank Equity:Opening-Balances
  ;;
  ;;      Used to silence the linter in multi-file beancount projects without
  ;;      dealing with multiple-include errors and redundancies.
  ;; REVIEW: PR features 1 and 2 upstream! 3 needs discussing.
  (advice-add #'flymake-bean-check--run :override #'+beancount--flymake-bean-check--run-a)

  ;; HACK: This enhances completion for beancount-mode in the following ways:
  ;;
  ;;   1. Adds completion for:
  ;;      - Event directives and values,
  ;;      - The payee field in transactions,
  ;;      - Currencies and commodities,
  ;;   2. Fixes completion for #tag and ^links not working at the end of a
  ;;      transaction's heading.
  ;;   3. Completion now scans not only the current file, but any included files
  ;;      (recursively) for candidates. See `+beancount-files' to configure
  ;;      this. This applies not only to completion-at-point functions, but also
  ;;      interactive commands like `beancount-insert-account'.
  ;; REVIEW: PR this upstream!
  (advice-add #'beancount-completion-at-point :override #'+beancount-completion-at-point-a)
  (advice-add #'beancount-get-account-names :override #'+beancount-get-account-names-a)

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
