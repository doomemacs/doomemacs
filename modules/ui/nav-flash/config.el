;;; ui/nav-flash/config.el -*- lexical-binding: t; -*-

(defvar +nav-flash-exclude-commands
  '(mouse-set-point evil-mouse-drag-region
    +org/dwim-at-point org-find-file org-find-file-at-mouse)
  "A list of commands that should not trigger nav-flash.")

(def-package! nav-flash
  :defer t
  :init
  ;; NOTE In :feature lookup `recenter' is hooked to a bunch of jumping
  ;; commands, which will trigger nav-flash.
  (add-hook!
    '(doom-enter-window-hook
      imenu-after-jump-hook evil-jumps-post-jump-hook
      counsel-grep-post-action-hook dumb-jump-after-jump-hook)
    #'+nav-flash|blink-cursor-maybe)

  ;; `org'
  (add-hook 'org-follow-link-hook #'+nav-flash|delayed-blink-cursor)

  ;; `saveplace'
  (advice-add #'save-place-find-file-hook :after #'+nav-flash*blink-cursor)

  ;; `evil'
  (advice-add #'evil-window-top    :after #'+nav-flash*blink-cursor)
  (advice-add #'evil-window-middle :after #'+nav-flash*blink-cursor)
  (advice-add #'evil-window-bottom :after #'+nav-flash*blink-cursor))

