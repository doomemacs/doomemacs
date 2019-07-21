;;; tools/prodigy/config.el -*- lexical-binding: t; -*-

(after! prodigy
  (set-evil-initial-state! 'prodigy-mode 'emacs)

  (advice-add #'prodigy-services :around #'+prodigy*services)

  (define-key prodigy-mode-map "d" #'+prodigy/delete))

