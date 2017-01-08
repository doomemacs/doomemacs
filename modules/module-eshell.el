;;; module-eshell.el --- -*- no-byte-compile: t; -*-

;; see:
;;   + `doom:eshell' (open in current buffer or popup)
;;   + `doom/eshell-tab' (open in separate tab)
;;   + `doom/eshell-frame' (open in separate frame)

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
  (def-popup! "^\\*eshell:popup\\*$" :regexp t :align below :size 25 :select t)

  (defun doom|eshell-keymap-setup ()
    "Setup eshell keybindings. This must be done in a hook because eshell
redefines its keys every time `eshell-mode' is enabled."
    (map! :map eshell-mode-map
          :n "i" 'doom/eshell-evil-prepend-maybe
          :n "I" 'doom/eshell-evil-prepend
          :n "a" 'doom/eshell-evil-append-maybe
          :n "A" 'doom/eshell-evil-append
          :n "r" 'doom/eshell-evil-replace-maybe
          :n "R" 'doom/eshell-evil-replace-state-maybe
          :n "c" 'doom/eshell-evil-change
          :n "C" 'doom/eshell-evil-change-line
          :i "<tab>" 'eshell-pcomplete
          :i "C-u" 'eshell-kill-input
          :i "SPC" 'self-insert-command
          :m "<return>" 'doom/eshell-evil-append
          :n [remap evil-window-split]       'doom/eshell-split
          :n [remap evil-window-vsplit]      'doom/eshell-vsplit
          :n [remap evil-record-macro]       'eshell-life-is-too-much
          [remap doom/close-window-or-tab]   'eshell-life-is-too-much))

  ;; Close window on exit
  (add-hook 'eshell-exit-hook 'doom|eshell-cleanup)
  (add-hook 'eshell-mode-hook 'doom|eshell-init)

  (add-hook 'eshell-mode-hook 'doom|eshell-keymap-setup)
  (add-hook 'eshell-mode-hook 'doom-hide-mode-line-mode)

  (add-hook! eshell-mode
    (add-hook 'evil-insert-state-exit-hook  'hl-line-mode nil t)
    (add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode -1)) nil t))

  ;; Aliases
  (setq eshell-command-aliases-list
        '(("q"   "exit")
          ("l"   "ls -1")
          ("ll"  "ls -l")
          ("la"  "ls -la")
          ("g"   "hub")
          ("gs"  "hub status --oneline .")
          ("gss" "hub status --oneline")))

  ;; Custom commands
  (defun eshell/e (file)
    (eshell-eval (cond ((doom/popup-p)
                        (doom/popup-save (find-file file))
                        0)
                       (t (find-file file)
                        0)))))

(provide 'module-eshell)
;;; module-eshell.el ends here
