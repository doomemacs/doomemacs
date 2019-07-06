;;; term/vterm/config.el -*- lexical-binding: t; -*-

(def-package! vterm
  :when (fboundp 'module-load)
  :defer t
  :preface (setq vterm-install t)
  :config
  (set-popup-rule! "^vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

  (add-hook 'vterm-mode-hook #'doom|mark-buffer-as-real)
  ;; Automatically kill buffer when vterm exits.
  (add-to-list 'vterm-exit-functions (lambda (buffer) (if buffer (kill-buffer buffer))))
  ;; Modeline serves no purpose in vterm
  (add-hook 'vterm-mode-hook #'hide-mode-line-mode)
  ;; Don't prompt about processes when killing vterm
  (setq-hook! 'vterm-mode-hook confirm-kill-processes nil))
