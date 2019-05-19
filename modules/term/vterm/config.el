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
  (setq-hook! 'vterm-mode-hook confirm-kill-processes nil)

  (when (featurep! :editor evil)
    (evil-set-initial-state 'vterm-mode 'insert)
    ;; Go back to normal state but don't move cursor backwards. Moving cursor
    ;; backwards is the default Vim behavior but it is not appropriate in some
    ;; cases like terminals.
    (setq-hook! 'vterm-mode-hook evil-move-cursor-back nil)
    ;; Those keys are commonly needed by terminals.
    (evil-define-key* 'insert vterm-mode-map
      (kbd "C-a") #'vterm--self-insert
      (kbd "C-b") #'vterm--self-insert ; Should not be necessary.
      (kbd "C-d") #'vterm--self-insert
      (kbd "C-e") #'vterm--self-insert
      (kbd "C-f") #'vterm--self-insert ; Should not be necessary.
      (kbd "C-k") #'vterm--self-insert
      (kbd "C-l") #'vterm--self-insert ; Should not be necessary.
      (kbd "C-n") #'vterm--self-insert
      (kbd "C-o") #'vterm--self-insert
      (kbd "C-p") #'vterm--self-insert
      (kbd "C-q") #'vterm--self-insert ; Should not be necessary.
      (kbd "C-r") #'vterm--self-insert
      (kbd "C-s") #'vterm--self-insert ; Should not be necessary.
      (kbd "C-t") #'vterm--self-insert
      (kbd "C-u") #'vterm--self-insert ; Should not be necessary.
      (kbd "C-v") #'vterm--self-insert ; Should not be necessary.
      (kbd "C-w") #'vterm--self-insert
      (kbd "C-y") #'vterm--self-insert
      (kbd "C-z") #'vterm--self-insert)))
