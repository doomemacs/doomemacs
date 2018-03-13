;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

;; Since Doom doesn't use `package-initialize', pkg-info won't get autoloaded
;; when `flycheck-version' needs it, so we need this:
(autoload 'pkg-info-version-info "pkg-info")

(def-package! flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
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

    ;; With the option of flychecking the buffer on escape, so we don't need
    ;; auto-flychecking on idle-change:
    (delq 'idle-change flycheck-check-syntax-automatically)))


;; Long story short, `flycheck-popup-tip' works everywhere but only looks *ok*.
;; `flycheck-pos-tip' looks great, but only in GUI Emacs on Linux. So we want:
;;
;; + GUI Emacs (Linux): pos-tip
;; + GUI Emacs (MacOS): popup-tip
;; + tty Emacs (anywhere): popup-tip

(def-package! flycheck-pos-tip
  :unless IS-MAC
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 10
        ;; fallback to flycheck-popup-tip in terminal Emacs
        flycheck-pos-tip-display-errors-tty-function
        #'flycheck-popup-tip-show-popup
        flycheck-display-errors-delay 0.7)
  (flycheck-pos-tip-mode))

(def-package! flycheck-popup-tip
  :commands (flycheck-popup-tip-mode flycheck-popup-tip-show-popup)
  :after flycheck
  :config (if IS-MAC (flycheck-popup-tip-mode)))

