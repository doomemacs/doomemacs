;;; ui/nav-flash/config.el -*- lexical-binding: t; -*-

(def-package! nav-flash
  :defer t
  :init
  ;; NOTE In :feature lookup `recenter' is hooked to a bunch of jumping
  ;; commands, which will trigger nav-flash.
  (add-hook!
    '(doom-enter-window-hook
      imenu-after-jump-hook evil-jumps-post-jump-hook
      counsel-grep-post-action-hook dumb-jump-after-jump-hook)
    #'+nav-flash/blink-cursor)

  ;; `saveplace'
  (advice-add #'save-place-find-file-hook :after #'+nav-flash/blink-cursor)

  ;; `evil'
  (advice-add #'evil-window-top    :after #'+nav-flash/blink-cursor)
  (advice-add #'evil-window-middle :after #'+nav-flash/blink-cursor)
  (advice-add #'evil-window-bottom :after #'+nav-flash/blink-cursor))

