;;; tools/flycheck/config.el -*- lexical-binding: t; -*-

(defvar +flycheck-lazy-idle-delay 3.0
  "The delay before flycheck checks the buffer, after a check that produces no
errors.")


;;
;;; Packages

(def-package! flycheck
  :commands flycheck-list-errors flycheck-buffer
  :after-call doom-switch-buffer-hook after-find-file
  :config
  ;; new-line checks are a mote excessive; idle checks are more than enough
  (delq! 'new-line flycheck-check-syntax-automatically)

  (add-hook 'doom-escape-hook
    (defun +flycheck-buffer-h ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))
    'append)

  (add-hook 'flycheck-after-syntax-check-hook
    (defun +flycheck-adjust-syntax-check-eagerness-h ()
      "Check for errors less often when there aren't any.
Done to reduce the load flycheck imposes on the current buffer."
      (if flycheck-current-errors
          (kill-local-variable 'flycheck-idle-change-delay)
        (setq-local flycheck-idle-change-delay +flycheck-lazy-idle-delay))))

  (global-flycheck-mode +1))


(def-package! flycheck-popup-tip
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :init (add-hook 'flycheck-mode-hook #'+flycheck-init-popups-h)
  :config
  (setq flycheck-popup-tip-error-prefix "✕ ")
  (after! evil
    ;; Don't display errors while in insert mode, as it can affect the cursor's
    ;; position or cause disruptive input delays.
    (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)))


(def-package! flycheck-posframe
  :when EMACS26+
  :when (featurep! +childframe)
  :defer t
  :init (add-hook 'flycheck-mode-hook #'+flycheck-init-popups-h)
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ "))
