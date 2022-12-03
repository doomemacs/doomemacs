;;; config/default/+emacs.el -*- lexical-binding: t; -*-

(require 'projectile) ; we need its keybinds immediately


;;
;;; Reasonable defaults

(setq shift-select-mode t)
(delete-selection-mode +1)

(use-package! expand-region
  :commands (er/contract-region er/mark-symbol er/mark-word)
  :config
  (defadvice! doom--quit-expand-region-a (&rest _)
    "Properly abort an expand-region region."
    :before '(evil-escape doom/escape)
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0))))


;;
;;; Keybinds

(when (modulep! +bindings)
  (load! "+emacs-bindings"))
