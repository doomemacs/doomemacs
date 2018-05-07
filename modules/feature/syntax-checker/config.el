;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

;; Since Doom doesn't use `package-initialize', pkg-info won't get autoloaded
;; when `flycheck-version' needs it, so we need this:
(autoload 'pkg-info-version-info "pkg-info")

(def-package! flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :config
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  ;; Popup
  (add-hook 'flycheck-mode-hook #'+syntax-checker-popup-mode)

  (after! evil
    (defun +syntax-checkers|flycheck-buffer ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))
    (add-hook 'doom-escape-hook #'+syntax-checkers|flycheck-buffer t)
    (add-hook 'evil-insert-state-exit-hook #'+syntax-checkers|flycheck-buffer)

    ;; With the option of flychecking the buffer on escape or leaving insert
    ;; mode, we don't need auto-flychecking on idle-change (which can feel slow,
    ;; esp on computers without SSDs).
    (delq 'idle-change flycheck-check-syntax-automatically)))


(def-package! flycheck-popup-tip
  :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup))


(def-package! flycheck-posframe
  :when EMACS26+
  :commands flycheck-posframe-show-posframe)
