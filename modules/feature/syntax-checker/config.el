;;; feature/syntax-checker/config.el

;; pkg-info doesn't get autoloaded when `flycheck-version' needs it, so we do
;; it ourselves:
(autoload 'pkg-info-version-info "pkg-info")

(def-package! flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :config
  ;; Emacs feels snappier without checks on idle/change
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (set! :popup " ?\\*Flycheck" :size 14 :noselect t :regexp t)

  (map! :map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)

  (after! evil
    ;; Flycheck buffer on ESC in normal mode.
    (defun +syntax-checkers|flycheck-buffer ()
      (if flycheck-mode (ignore-errors (flycheck-buffer))))
    (add-hook '+evil-esc-hook #'+syntax-checkers|flycheck-buffer)))


(def-package! flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1))
