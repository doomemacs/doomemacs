;;; core-flycheck.el --- check yourself before you shrek yourself

(use-package flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :init
  (setq flycheck-indication-mode 'right-fringe
        ;; Removed checks on idle/change for snappiness
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc make))
  :config
  (require 'flycheck-package)
  (flycheck-package-setup)

  (require 'flycheck-pos-tip)
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1)

  (evil-initial-state 'flycheck-error-list-mode 'emacs)
  (map! :map flycheck-error-list-mode-map
        :n [escape] 'kill-this-buffer
        :n "q"      'kill-this-buffer
        :n "C-n"    'flycheck-error-list-next-error
        :n "C-p"    'flycheck-error-list-previous-error
        :n "j"      'flycheck-error-list-next-error
        :n "k"      'flycheck-error-list-previous-error
        :n "RET"    'flycheck-error-list-goto-error)

  ;; And on ESC in normal mode.
  (advice-add 'evil-force-normal-state :after 'narf*flycheck-buffer)

  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 8 24 56 120 248 120 56 24 8 0 0 0]
    ;; (fringe-helper-convert
    ;;  "........"
    ;;  "........"
    ;;  "........"
    ;;  "....X..."
    ;;  "...XX..."
    ;;  "..XXX..."
    ;;  ".XXXX..."
    ;;  "XXXXX..."
    ;;  ".XXXX..."
    ;;  "..XXX..."
    ;;  "...XX..."
    ;;  "....X..."
    ;;  "........"
    ;;  "........"
    ;;  "........")
    ))

(use-package flyspell :commands flyspell-mode)

(provide 'core-flycheck)
;;; core-flycheck.el ends here
