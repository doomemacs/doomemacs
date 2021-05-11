;;; lang/beancount/config.el -*- lexical-binding: t; -*-

(use-package! beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :init
  (add-hook 'beancount-mode-hook #'outline-minor-mode)

  (after! all-the-icons
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.beancount\\'" all-the-icons-material "attach_money" :face all-the-icons-lblue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(beancount-mode all-the-icons-material "attach_money" :face all-the-icons-lblue)))

  ;; TODO PR this upstream
  (defadvice! +beancount--fix-faces-a ()
    "`beancount-outline-face' returns org-level-N faces for beancount-mode's
font-lock keywords, but there is no guarantee `org' will be loaded when
beancount-mode is activated. If it is absent, you will see a flood of 'invalid
face' errors. Just use the outline-N faces to get around this.

This msut be advised *before* beancount-mode loads, because
`beancount-outline-face' is used at load time."
    :override #'beancount-outline-face
    (if outline-minor-mode
        (let ((level (funcall outline-level)))
          (if (integerp level)
              (intern (format "outline-%d" level))))
      nil))

  :config
  (when (featurep! +lsp)
    (after! lsp-mode
      ;; TODO PR this upstream
      (add-to-list 'lsp-language-id-configuration '(beancount-mode . "beancount"))
      (defvar lsp-beancount-langserver-executable "beancount-langserver")
      (defvar lsp-beancount-journal-file nil)
      (defvar lsp-beancount-python-interpreter
        (or (executable-find "python3")
            (executable-find "python")))
      (lsp-register-client
       (make-lsp-client :new-connection
                        (lsp-stdio-connection `(,lsp-beancount-langserver-executable "--stdio"))
                        :major-modes '(beancount-mode)
                        :initialization-options
                        `((journalFile . ,lsp-beancount-journal-file)
                          (pythonPath . ,lsp-beancount-python-interpreter))
                        :server-id 'beancount-ls)))
    (add-hook 'beancount-mode-local-vars-hook #'lsp!))

  (setq beancount-electric-currency t)

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
         "a" #'beancount-insert-account
         "p" #'beancount-insert-prices
         "d" #'beancount-insert-date))

  ;; TODO PR this upstream?
  (defun +beancount-electric-align-h ()
    "Align amount in current line to `beancount-number-alignment-column'."
    (when (and beancount-electric-currency (eq last-command-event ?\n))
      (save-excursion
        (forward-line -1)
        (ignore-errors (beancount-align-to-previous-number)))))
  (add-hook! 'beancount-mode-hook
    (add-hook 'post-self-insert-hook #'+beancount-electric-align-h 'append t))

  ;; TODO PR this upstream
  (defadvice! +beancount--fix-account-currency-a (account)
    "Fixes `beancount-electric-currency', which could never find the currency
for the current account. Also allows it to fall back to the operating_currency."
    :override #'beancount--account-currency
    (save-excursion
      (goto-char (point-min))
      (when (or (re-search-forward
                 (concat "^" beancount-date-regexp " +open"
                         "\\s-+" (regexp-quote account)
                         "\\s-+\\(" beancount-currency-regexp "\\)\\>")
                 nil t)
                (re-search-forward
                 (concat "^option\\s-+\"operating_currency\"\\s-+\"\\("
                         beancount-currency-regexp
                         "\\)\"")
                 nil t))
        (match-string-no-properties 1)))))
