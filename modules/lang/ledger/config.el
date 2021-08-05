;;; lang/ledger/config.el -*- lexical-binding: t; -*-

(use-package! ledger-mode
  :defer t
  :init
  (setq ledger-clear-whole-transactions 1
        ledger-mode-should-check-version nil)

  (add-hook 'ledger-mode-hook #'outline-minor-mode)

  (set-company-backend! 'ledger-mode 'company-capf)
  (set-popup-rules!
    '(("^\\*Ledger Report" :size 0.5 :quit 'other :ttl 0)
      ("^\\*Ledger Error"  :quit t :ttl 0)))

  (defadvice! +ledger--fail-gracefully-if-absent-a (fn)
    "Fail gracefully if ledger binary isn't available."
    :around #'ledger-check-version
    (if (executable-find ledger-binary-path)
        (funcall fn)
      (message "Couldn't find '%s' executable" ledger-binary-path)))

  ;; `ledger-mode' lacks imenu support out of the box, so we gie it some. At
  ;; least to make jumping to outline headings or transactions easier.
  (setq-hook! 'ledger-mode-hook
    imenu-generic-expression
    `((nil ,(concat
             "^[\\*]+[ \t]+\\([^\n\r]+\\)\\|"                              ; outline headings
             "^[0-9]\\{4\\}[-/.][0-9]\\{2\\}[-/.][0-9]\\{2\\}[ \t]+[^\n]+")  ; transactions
           0)))

  (map! :map ledger-report-mode-map
        "C-c C-c" #'ledger-report-edit-report
        "C-c C-r" #'ledger-report-redo
        "C-c C-s" #'ledger-report-save
        :map ledger-reconcile-mode-map
        [tab] #'ledger-reconcile-toggle))


(use-package! flycheck-ledger
  :when (featurep! :checkers syntax)
  :after ledger-mode)


(use-package! evil-ledger
  :when (featurep! :editor evil +everywhere)
  :hook (ledger-mode . evil-ledger-mode)
  :config
  (set-evil-initial-state! 'ledger-report-mode 'normal)
  (map! (:map ledger-report-mode-map
         :n "q"   #'ledger-report-quit
         :n "RET" #'ledger-report-edit-report
         :n "gd"  #'ledger-report-visit-source
         :n "gr"  #'ledger-report-redo
         ;; This is redundant, but helps `substitute-command-keys' find them
         ;; below, in `+ledger--fix-key-help-a'.
         :n "C-d"  #'evil-scroll-down
         :n "C-u"  #'evil-scroll-up)
        (:map ledger-mode-map
         :m "]]" #'ledger-navigate-next-xact-or-directive
         :m "[[" #'ledger-navigate-prev-xact-or-directive)

        (:localleader
         :map ledger-mode-map
         "a" #'ledger-add-transaction
         "e" #'ledger-post-edit-amount
         "t" #'ledger-toggle-current
         "d" #'ledger-delete-current-transaction
         "r" #'ledger-report
         "R" #'ledger-reconcile
         "s" #'ledger-sort-region
         "S" #'ledger-schedule-upcoming
         (:prefix "g"
          "s" #'ledger-display-ledger-stats
          "b" #'ledger-display-balance-at-point)))

  (defadvice! +ledger--fix-key-help-a (fn &rest args)
    "Fix inaccurate keybind message."
    :around #'ledger-report
    (quiet! (apply fn args))
    (with-current-buffer (get-buffer ledger-report-buffer-name)
      (setq header-line-format
            (substitute-command-keys
             (concat "\\[ledger-report-quit] to quit; "
                     "\\[ledger-report-redo] to redo; "
                     "\\[ledger-report-edit-report] to edit; "
                     "\\[ledger-report-save] to save; "
                     "\\[evil-scroll-up] and \\[evil-scroll-down] to scroll"))))))
