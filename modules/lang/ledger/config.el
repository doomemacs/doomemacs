;;; lang/ledger/config.el -*- lexical-binding: t; -*-

;; `ledger-mode'
(setq ledger-clear-whole-transactions 1)

(defun +ledger*check-version (orig-fn)
  "Fail gracefully if ledger binary isn't available."
  (if (executable-find ledger-binary-path)
      (funcall orig-fn)
    (message "Couldn't find '%s' executable" ledger-binary-path)))
(advice-add #'ledger-check-version :around #'+ledger*check-version)

;; Restore leader key in ledger reports
(map! :after ledger-mode
      :map ledger-report-mode-map
      "C-c C-c" #'ledger-report-edit-report
      "C-c C-r" #'ledger-report-redo
      "C-c C-s" #'ledger-report-save
      :map ledger-reconcile-mode-map
      [tab] #'ledger-reconcile-toggle)


(def-package! flycheck-ledger
  :when (featurep! :feature syntax-checker)
  :after ledger-mode)


(def-package! evil-ledger
  :when (featurep! :feature evil +everywhere)
  :hook (ledger-mode . evil-ledger-mode)
  :config
  (set-evil-initial-state! 'ledger-report-mode 'normal)
  (map! :map ledger-report-mode-map
        :n "q"   #'ledger-report-quit
        :n "RET" #'ledger-report-edit-report
        :n "gd"  #'ledger-report-visit-source
        :n "gr"  #'ledger-report-redo
        :map ledger-mode-map
        :m "]]" #'ledger-navigate-next-xact-or-directive
        :m "[[" #'ledger-navigate-prev-xact-or-directive

        :localleader
        :map ledger-mode-map
        "a" #'ledger-add-transaction
        "t" #'ledger-toggle-current
        "d" #'ledger-delete-current-transaction
        "r" #'ledger-report
        "R" #'ledger-reconcile
        "s" #'ledger-sort-region
        "S" #'ledger-schedule-upcoming
        (:prefix "g"
          "s" #'ledger-display-ledger-stats
          "b" #'ledger-display-balance-at-point))
  ;; Fix inaccurate keybind message
  (defun +ledger*fix-key-help (&rest _)
    (message "q to quit; gr to redo; RET to edit; C-c C-s to save"))
  (advice-add #'ledger-report :after #'+ledger*fix-key-help))
