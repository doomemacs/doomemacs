;;; core-flycheck.el --- check yourself before you shrek yourself
;; Related to: lib/defuns-flycheck.el

(use-package flycheck
  :functions (flycheck-buffer)
  :commands (flycheck-mode flycheck-list-errors)
  :init
  (setq flycheck-indication-mode nil
        ;; Removed checks on idle/change for snappiness
        flycheck-check-syntax-automatically '(save mode-enabled idle-change)
        flycheck-disabled-checkers '(emacs-lisp-checkdoc make))
  :config
  (bind! :map flycheck-error-list-mode-map
         :n [escape] 'kill-this-buffer
         :n "q"      'kill-this-buffer)

  (evil-initial-state 'flycheck-error-list-mode 'emacs)

  ;; Check buffer when normal mode is entered
  (add-hook! evil-normal-state-entry 'narf*flycheck-buffer)
  ;; And on ESC in normal mode.
  (advice-add 'evil-force-normal-state :after 'narf*flycheck-buffer)
  (advice-add 'flycheck-mode-line-status-text :filter-return 'narf*fly-shorter-status))

(use-package flyspell :commands flyspell-mode)

(provide 'core-flycheck)
;;; core-flycheck.el ends here
