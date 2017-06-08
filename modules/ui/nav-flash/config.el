;;; ui/nav-flash/config.el -*- lexical-binding: t; -*-

(def-package! nav-flash
  :commands nav-flash-show
  :init
  ;; NOTE In :feature jump `recenter' is hooked to a bunch of jumping commands,
  ;; which will trigger nav-flash.
  (advice-add #'windmove-do-window-select :around #'+doom*blink-cursor-maybe)
  (advice-add #'recenter :around #'+doom*blink-cursor-maybe)

  (after! evil
    (advice-add #'evil-window-top    :after #'+doom/blink-cursor)
    (advice-add #'evil-window-middle :after #'+doom/blink-cursor)
    (advice-add #'evil-window-bottom :after #'+doom/blink-cursor)))


