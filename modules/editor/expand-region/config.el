;;; editor/expand-region/config.el -*- lexical-binding: t; -*-

(def-package! expand-region
  :commands (er/contract-region er/mark-symbol er/mark-word)
  :config
  (defun doom*quit-expand-region ()
    "Properly abort an expand-region region."
    (when (memq last-command '(er/expand-region er/contract-region))
      (er/contract-region 0)))
  (advice-add #'evil-escape :before #'doom*quit-expand-region)
  (advice-add #'doom/escape :before #'doom*quit-expand-region))
