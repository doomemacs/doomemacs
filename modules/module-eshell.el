;;; module-eshell.el --- -*- no-byte-compile: t; -*-

(use-package eshell
  :init
  (setq eshell-directory-name (concat doom-temp-dir "/eshell")
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-buffer-shorthand t
        ;; em-prompt
        eshell-prompt-function 'doom/eshell-prompt
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t
        ;; em-alias
        eshell-aliases-file (concat doom-temp-dir "/.eshell-aliases"))

  :config
  (evil-set-initial-state 'eshell-mode 'insert)

  (defun doom|eshell-keymap-setup ()
    (map! :map eshell-mode-map
          :n "i" 'doom/eshell-evil-prepend-maybe
          :n "I" 'doom/eshell-evil-prepend
          :n "a" 'doom/eshell-evil-append-maybe
          :n "A" 'doom/eshell-evil-append
          :n "r" 'doom/eshell-evil-replace-maybe
          :n "R" 'doom/eshell-evil-replace-state-maybe
          :i "C-u" 'eshell-kill-input
          :i "SPC" 'self-insert-command
          :m "<return>" 'doom/eshell-evil-append
          :n [remap doom/evil-window-split] 'doom/eshell-split
          :n [remap doom/evil-window-vsplit] 'doom/eshell-vsplit))

  (defun doom|eshell-init ()
    (when (eq major-mode 'eshell-mode)
      (add-to-list 'doom-eshell-buffers (current-buffer))))

  (defun doom|eshell-cleanup ()
    (when (eq major-mode 'eshell-mode)
      (setq doom-eshell-buffers (delete (current-buffer) doom-eshell-buffers))
      (delete-window)))

  ;; Close window on exit
  (add-hook 'eshell-exit-hook 'doom|eshell-cleanup)
  (add-hook 'eshell-mode-hook 'doom|eshell-init)

  (add-hook 'eshell-mode-hook 'doom|eshell-keymap-setup)
  (add-hook 'eshell-mode-hook 'doom-hide-mode-line-mode)
  (add-hook 'eshell-mode-hook 'hl-line-mode))

(provide 'module-eshell)
;;; module-eshell.el ends here
