;;; tools/flycheck/config.el -*- lexical-binding: t; -*-

(defvar +flycheck-on-escape t
  "If non-nil, flycheck will recheck the current buffer when pressing ESC/C-g.")


;;
;;; Packages

(def-package! flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :after-call (doom-switch-buffer-hook after-find-file)
  :config
  ;; new-line checks are a mote excessive; idle checks are more than enough
  (setq flycheck-check-syntax-automatically
        (delq 'new-line flycheck-check-syntax-automatically))

  (defun +flycheck|buffer ()
    "Flycheck buffer on ESC in normal mode."
    (when (and flycheck-mode +flycheck-on-escape)
      (ignore-errors (flycheck-buffer))
      nil))
  (add-hook 'doom-escape-hook #'+flycheck|buffer t)

  (defun +flycheck|adjust-syntax-check-eagerness ()
    "Check for errors less often when there aren't any.
Done to reduce the load flycheck imposes on the current buffer."
    (if flycheck-current-errors
        (kill-local-variable 'flycheck-idle-change-delay)
      (setq-local flycheck-idle-change-delay 3.0)))
  (add-hook 'flycheck-after-syntax-check-hook #'+flycheck|adjust-syntax-check-eagerness)

  (global-flycheck-mode +1))


(def-package! flycheck-popup-tip
  :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
  :init (add-hook 'flycheck-mode-hook #'+flycheck|init-popups)
  :config
  (setq flycheck-popup-tip-error-prefix "✕ ")
  (after! evil
    ;; Don't display errors while in insert mode, as it can affect the cursor's
    ;; position or cause disruptive input delays.
    (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)))


(def-package! flycheck-posframe
  :when (and EMACS26+ (featurep! +childframe))
  :defer t
  :init (add-hook 'flycheck-mode-hook #'+flycheck|init-popups)
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ "))
