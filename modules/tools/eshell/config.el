;;; tools/eshell/config.el -*- lexical-binding: t; -*-

;; This is highly experimental. I don't use eshell often, so this may need work.

;; see:
;;   + `+eshell/run': open in current buffer
;;   + `+eshell/tab': open in separate tab (requires :feature workspaces)
;;   + `+eshell/popup': open in a popup

(defvar +eshell-buffers '()
  "TODO")

(def-package! eshell ; built-in
  :init
  (setq eshell-directory-name (concat doom-cache-dir "/eshell")
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell/prompt
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t
        ;; em-alias
        eshell-aliases-file (concat doom-local-dir ".eshell-aliases"))

  :config
  (set! :popup "^\\*eshell:popup\\*$" :regexp t :size 25)
  (set! :evil-state 'eshell-mode 'insert)

  (after! em-term
    ;; Visual commands require a proper terminal. Eshell can't handle that, so it
    ;; delegates these commands to a term buffer.
    (nconc eshell-visual-commands '("tmux" "htop" "bash" "zsh" "fish" "vim" "nvim"))
    (setq eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))))

  (defun +eshell|keymap-setup ()
    "Setup eshell keybindings. This must be done in a hook because eshell
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
          :i "<tab>"    #'eshell-pcomplete
          :i "C-u"      #'eshell-kill-input
          :i "SPC"      #'self-insert-command
          :i "C-a"      #'eshell-bol
          :i "C-d"      #'+eshell/quit-or-delete-char
          :i "C-k"      #'kill-line
          :i "C-p"      #'eshell-previous-input
          :i "<up>"     #'eshell-previous-input
          :i "C-n"      #'eshell-previous-input
          :i "<down>"   #'eshell-previous-input
          :m "<return>" #'+eshell/evil-append
          :n [remap evil-window-split]       #'+eshell/split
          :n [remap evil-window-vsplit]      #'+eshell/vsplit
          :n [remap evil-record-macro]       #'eshell-life-is-too-much
          [remap doom/close-window-or-tab]   #'eshell-life-is-too-much))
  (add-hook 'eshell-mode-hook #'+eshell|keymap-setup)

  (defun +eshell|cleanup ()
    "Close window (or workspace) on quit."
    (setq +eshell-buffers (delete (current-buffer) +eshell-buffers))
    (cond ((doom-popup-p)
           (delete-window))
          ((and (featurep! :feature workspaces)
                (string= "eshell" (+workspace-current-name)))
           (+workspace/close-window-or-workspace))))
  (add-hook 'eshell-exit-hook #'+eshell|cleanup)

  (defun +eshell|init ()
    "Keep track of eshell buffers."
    (add-to-list '+eshell-buffers (current-buffer)))
  (add-hook 'eshell-mode-hook #'+eshell|init)

  (add-hook! eshell-mode
    (add-hook 'evil-insert-state-exit-hook  #'hl-line-mode nil t)
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
  ;; (defun eshell/e (file)
  ;;   (eshell-eval (cond ((doom/popup-p)
  ;;                       (doom/popup-save (find-file file))
  ;;                       0)
  ;;                      (t (find-file file)
  ;;                       0))))
  )

