;;; core-flycheck.el --- check yourself before you shrek yourself

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :init
  (setq flycheck-indication-mode 'right-fringe
        ;; Removed checks on idle/change for snappiness
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-highlighting-mode 'symbols
        flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc make)
        ;; `flycheck-pos-tip'
        flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)

  :config
  (def-popup! " ?\\*Flycheck.+\\*" :align below :size 14 :noselect t :regexp t)

  (map! :map flycheck-error-list-mode-map
        :n "C-n" 'flycheck-error-list-next-error
        :n "C-p" 'flycheck-error-list-previous-error
        :n "j"   'flycheck-error-list-next-error
        :n "k"   'flycheck-error-list-previous-error
        :n "RET" 'flycheck-error-list-goto-error)

  ;; Flycheck buffer on ESC in normal mode.
  (advice-add 'evil-force-normal-state :after 'doom*flycheck-buffer)

  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])

  (require 'flycheck-package)
  (flycheck-package-setup)

  (when (eq window-system 'mac)
    (require 'flycheck-pos-tip)
    (flycheck-pos-tip-mode +1)))

(use-package flyspell :commands flyspell-mode)

(provide 'core-flycheck)
;;; core-flycheck.el ends here
