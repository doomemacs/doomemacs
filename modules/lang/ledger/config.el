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
  (define-key! (ledger-report-mode-map ledger-reconcile-mode-map)
    doom-leader-key nil)
  (define-key ledger-reconcile-mode-map [tab] #'ledger-reconcile-toggle))


(def-package! evil-ledger
  :when (featurep! :feature evil)
  :hook (ledger-mode . evil-ledger-mode))


(def-package! flycheck-ledger
  :when (featurep! :feature syntax-checker)
  :after ledger-mode)
