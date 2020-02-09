;;; term/vterm/config.el -*- lexical-binding: t; -*-

(use-package! vterm
  :when module-file-suffix
  :defer t
  :preface (setq vterm-install t)
  :config
  (set-popup-rule! "^vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

  (setq-hook! 'vterm-mode-hook
    ;; Don't prompt about processes when killing vterm
    confirm-kill-processes nil
    ;; Prevent premature horizontal scrolling
    hscroll-margin 0)

  (setq vterm-kill-buffer-on-exit t)

  (add-hook 'vterm-mode-hook #'doom-mark-buffer-as-real-h)
  ;; Modeline serves no purpose in vterm
  (add-hook 'vterm-mode-hook #'hide-mode-line-mode))
