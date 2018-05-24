;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

(def-package! flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :config
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  (after! evil
    (defun +syntax-checkers|flycheck-buffer ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))
    (add-hook 'doom-escape-hook #'+syntax-checkers|flycheck-buffer t)
    (add-hook 'evil-insert-state-exit-hook #'+syntax-checkers|flycheck-buffer)))


(def-package! flycheck-popup-tip
  :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
  :init (add-hook 'flycheck-mode-hook #'+syntax-checker-popup-mode))


(def-package! flycheck-posframe
  :when (and EMACS26+ (featurep! +childframe))
  :commands flycheck-posframe-show-posframe
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ "))
