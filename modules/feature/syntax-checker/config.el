;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

;; pkg-info doesn't get autoloaded when `flycheck-version' needs it, so we do
;; it ourselves:
(autoload 'pkg-info-version-info "pkg-info")

(def-package! flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :config
  ;; Emacs feels snappier without checks on idle/change
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (set! :popup 'flycheck-error-list-mode :select t :autokill t)

  (after! evil
    ;; Flycheck buffer on ESC in normal mode.
    (defun +syntax-checkers|flycheck-buffer ()
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))
    (add-hook '+evil-esc-hook #'+syntax-checkers|flycheck-buffer t)))


(def-package! flycheck-pos-tip
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        flycheck-display-errors-delay 0.5)
  (flycheck-pos-tip-mode +1))

