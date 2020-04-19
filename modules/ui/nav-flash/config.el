;;; ui/nav-flash/config.el -*- lexical-binding: t; -*-

(defvar +nav-flash-exclude-commands
  '(mouse-set-point mouse-drag-region evil-mouse-drag-region +org/dwim-at-point
    org-find-file org-find-file-at-mouse)
  "A list of commands that should not trigger nav-flash.")


;;
;;; Packages

(use-package! nav-flash
  :defer t
  :init
  ;; NOTE In :tools lookup `recenter' is hooked to a bunch of jumping
  ;; commands, which will trigger nav-flash.
  (add-hook! '(imenu-after-jump-hook
               better-jumper-post-jump-hook
               counsel-grep-post-action-hook
               dumb-jump-after-jump-hook)
             #'+nav-flash-blink-cursor-maybe-h)

  (add-hook 'doom-switch-window-hook #'+nav-flash-blink-cursor-maybe-h)

  ;; `org'
  (add-hook 'org-follow-link-hook #'+nav-flash-delayed-blink-cursor-h)

  ;; `saveplace'
  (advice-add #'save-place-find-file-hook :after #'+nav-flash-blink-cursor-a)

  ;; `evil'
  (advice-add #'evil-window-top    :after #'+nav-flash-blink-cursor-a)
  (advice-add #'evil-window-middle :after #'+nav-flash-blink-cursor-a)
  (advice-add #'evil-window-bottom :after #'+nav-flash-blink-cursor-a)

  ;; Bound to `ga' for evil users
  (advice-add #'what-cursor-position :after #'+nav-flash-blink-cursor-a)

  :config
  (if EMACS27+ (set-face-extend 'nav-flash-face t)))
