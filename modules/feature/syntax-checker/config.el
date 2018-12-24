;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

(def-package! flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :after-call (doom-enter-buffer-hook after-find-file)
  :config
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  (after! evil
    (defun +syntax-checkers|flycheck-buffer ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))
    (add-hook 'doom-escape-hook #'+syntax-checkers|flycheck-buffer t))

  (global-flycheck-mode +1))


(def-package! flycheck-popup-tip
  :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
  :init (add-hook 'flycheck-mode-hook #'+syntax-checker-popup-mode)
  :config (setq flycheck-popup-tip-error-prefix "✕ "))


(def-package! flycheck-posframe
  :when (and EMACS26+ (featurep! +childframe))
  :commands flycheck-posframe-show-posframe
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ "))
