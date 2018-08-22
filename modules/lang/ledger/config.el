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
(after! ledger-mode
  (define-key! ledger-report-mode-map
    (kbd "C-c C-c") #'ledger-report-edit-report
    (kbd "C-c C-r") #'ledger-report-redo
    (kbd "C-c C-s") #'ledger-report-save)
  (define-key ledger-reconcile-mode-map
    [tab] #'ledger-reconcile-toggle))


(def-package! flycheck-ledger
  :when (featurep! :feature syntax-checker)
  :after ledger-mode)


(def-package! evil-ledger
  :when (featurep! :feature evil +everywhere)
  :hook (ledger-mode . evil-ledger-mode)
  :config
  (set-evil-initial-state! 'ledger-report-mode 'normal)
  (evil-define-key* 'normal ledger-report-mode-map
    "q"   #'ledger-report-quit
    "RET" #'ledger-report-edit-report
    "gd"  #'ledger-report-visit-source
    "gr"  #'ledger-report-redo)
  (evil-define-key* 'motion ledger-mode-map
    "]]" #'ledger-navigate-next-xact-or-directive
    "[[" #'ledger-navigate-prev-xact-or-directive)
  (map! :map ledger-mode-map
        :localleader
        :n "a" #'ledger-add-transaction
        :n "t" #'ledger-toggle-current
        :n "d" #'ledger-delete-current-transaction
        :n "r" #'ledger-report
        :n "R" #'ledger-reconcile
        :n "S" #'ledger-schedule-upcoming
        :v "s" #'ledger-sort-region
        (:prefix "g"
          :n "s" #'ledger-display-ledger-stats
          :n "b" #'ledger-display-balance-at-point))
  ;; Fix inaccurate keybind message
  (defun +ledger*fix-key-help (&rest _)
    (message "q to quit; gr to redo; RET to edit; C-c C-s to save"))
  (advice-add #'ledger-report :after #'+ledger*fix-key-help))
