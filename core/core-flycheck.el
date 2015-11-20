;;; core-flycheck.el --- check yourself before you shrek yourself
;; Related to: lib/defuns-flycheck.el

(use-package flycheck
  :diminish flycheck-mode
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :init
  (setq flycheck-indication-mode 'right-fringe
        ;; Removed checks on idle/change for snappiness
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-disabled-checkers '(emacs-lisp-checkdoc make))
  :config
  (bind! :map flycheck-error-list-mode-map
         :n [escape] 'kill-this-buffer
         :n "q"      'kill-this-buffer
         :n "C-n"    'flycheck-error-list-next-error
         :n "C-p"    'flycheck-error-list-previous-error
         :n "j"      'flycheck-error-list-next-error
         :n "k"      'flycheck-error-list-previous-error
         :n "RET"    'flycheck-error-list-goto-error)

  (evil-initial-state 'flycheck-error-list-mode 'emacs)

  ;; Check buffer when normal mode is entered
  (add-hook! evil-normal-state-entry 'narf*flycheck-buffer)
  ;; And on ESC in normal mode.
  (advice-add 'evil-force-normal-state :after 'narf*flycheck-buffer))

(use-package flyspell :commands flyspell-mode)

(provide 'core-flycheck)
;;; core-flycheck.el ends here
