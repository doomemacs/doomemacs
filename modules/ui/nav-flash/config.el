;;; ui/nav-flash/config.el -*- lexical-binding: t; -*-

(def-package! nav-flash
  :commands nav-flash-show
  :init
  ;; NOTE In :feature jump `recenter' is hooked to a bunch of jumping commands,
  ;; which will trigger nav-flash.
  (add-hook 'doom-after-switch-window-hook #'+nav-flash/blink-cursor)
  (advice-add #'recenter :around #'+nav-flash*blink-cursor-maybe)

  (advice-add #'save-place-find-file-hook :after #'+nav-flash/blink-cursor)

  (after! evil
    (advice-add #'evil--jumps-jump   :after #'+nav-flash/blink-cursor)

    (advice-add #'evil-window-top    :after #'+nav-flash/blink-cursor)
    (advice-add #'evil-window-middle :after #'+nav-flash/blink-cursor)
    (advice-add #'evil-window-bottom :after #'+nav-flash/blink-cursor)))


