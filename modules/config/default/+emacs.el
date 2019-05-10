;;; config/default/+emacs.el -*- lexical-binding: t; -*-

(require 'projectile) ; we need its keybinds immediately


;;
;;; Reasonable defaults

(setq shift-select-mode t)
(delete-selection-mode +1)

(def-package! expand-region
  :commands (er/contract-region er/mark-symbol er/mark-word)
  :config
  (defun doom*quit-expand-region ()
    "Properly abort an expand-region region."
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0)))
  (advice-add #'evil-escape :before #'doom*quit-expand-region)
  (advice-add #'doom/escape :before #'doom*quit-expand-region))


;;
;;; Keybinds

(when (featurep! +bindings)
  (load! "+emacs-bindings"))
