;;; tools/flycheck/config.el -*- lexical-binding: t; -*-

(defvar +flycheck-on-escape t
  "If non-nil, flycheck will recheck the current buffer when pressing ESC/C-g.")


;;
;; Packages

(def-package! flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :after-call (doom-switch-buffer-hook after-find-file)
  :config
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically (delq 'new-line flycheck-check-syntax-automatically))

  (defun +flycheck|buffer ()
    "Flycheck buffer on ESC in normal mode."
    (when (and flycheck-mode +flycheck-on-escape)
      (ignore-errors (flycheck-buffer))
      nil))
  (add-hook 'doom-escape-hook #'+flycheck|buffer t)

  (after! evil
    (setq-hook! 'evil-insert-state-entry-hook
      flycheck-idle-change-delay 1.75)
    (setq-hook! 'evil-insert-state-exit-hook
      flycheck-idle-change-delay (default-value 'flycheck-idle-change-delay)))

  (global-flycheck-mode +1))


(def-package! flycheck-popup-tip
  :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
  :init (add-hook 'flycheck-mode-hook #'+flycheck|init-popups)
  :config (setq flycheck-popup-tip-error-prefix "✕ "))


(def-package! flycheck-posframe
  :when (and EMACS26+ (featurep! +childframe))
  :defer t
  :init (add-hook 'flycheck-mode-hook #'+flycheck|init-popups)
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ "))
