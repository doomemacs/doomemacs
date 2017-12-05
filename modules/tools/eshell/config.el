;;; tools/eshell/config.el -*- lexical-binding: t; -*-

;; This is highly experimental. I don't use eshell often, so this may need work.

;; see:
;;   + `+eshell/open': open in current buffer
;;   + `+eshell/open-popup': open in a popup
;;   + `+eshell/open-workspace': open in separate tab (requires :feature workspaces)

(def-package! eshell ; built-in
  :commands eshell-mode
  :init
  (setq eshell-directory-name (concat doom-etc-dir "/eshell")
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-prompt
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t
        ;; em-alias
        eshell-aliases-file (concat doom-local-dir ".eshell-aliases"))

  :config
  (set! :evil-state 'eshell-mode 'insert)

  ;; Keep track of open eshell buffers
  (add-hook 'eshell-mode-hook #'+eshell|init)
  (add-hook 'eshell-exit-hook #'+eshell|cleanup)

  (after! em-term
    ;; Visual commands require a proper terminal. Eshell can't handle that, so it
    ;; delegates these commands to a term buffer.
    (nconc eshell-visual-commands '("tmux" "htop" "bash" "zsh" "fish" "vim" "nvim"))
    (setq eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))))

  (defun +eshell|init-keymap ()
    "Setup eshell keybindings. This must be done in a hook because eshell-mode
redefines its keys every time `eshell-mode' is enabled."
    (map! :map eshell-mode-map
          :n "i"        #'+eshell/evil-prepend-maybe
          :n "I"        #'+eshell/evil-prepend
          :n "a"        #'+eshell/evil-append-maybe
          :n "A"        #'+eshell/evil-append
          :n "r"        #'+eshell/evil-replace-maybe
          :n "R"        #'+eshell/evil-replace-state-maybe
          :n "c"        #'+eshell/evil-change
          :n "C"        #'+eshell/evil-change-line
          :i [tab]      #'eshell-pcomplete
          :i "SPC"      #'self-insert-command
          :i "C-u"      #'eshell-kill-input
          :i "C-a"      #'eshell-bol
          :i "C-d"      #'+eshell/quit-or-delete-char
          :i "C-k"      #'kill-line
          :i "C-p"      #'eshell-previous-input
          :i "<up>"     #'eshell-previous-input
          :i "C-n"      #'eshell-next-input
          :i "<down>"   #'eshell-next-input
          :m "<return>" #'+eshell/evil-append
          :n [remap evil-window-split]     #'+eshell/split
          :n [remap evil-window-vsplit]    #'+eshell/vsplit
          :n [remap evil-record-macro]     #'eshell-life-is-too-much
          [remap kill-this-buffer] #'eshell-life-is-too-much
          [remap +workspace/close-window-or-workspace] #'eshell-life-is-too-much))
  (add-hook 'eshell-mode-hook #'+eshell|init-keymap)

  ;; Aliases
  (setq eshell-command-aliases-list
        '(("q"   "exit")
          ("l"   "ls -1")
          ("ll"  "ls -l")
          ("la"  "ls -la")
          ("g"   "hub")
          ("gs"  "hub status --short ."))))

