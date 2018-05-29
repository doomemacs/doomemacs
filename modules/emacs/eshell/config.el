;;; emacs/eshell/config.el -*- lexical-binding: t; -*-

;; see:
;;   + `+eshell/open': open in current buffer
;;   + `+eshell/open-popup': open in a popup
;;   + `+eshell/open-workspace': open in separate tab (requires :feature
;;     workspaces)

(def-package! eshell ; built-in
  :defer t
  :init
  (setq eshell-directory-name
        (let ((dir (expand-file-name "eshell" doom-private-dir)))
          (if (file-directory-p dir)
              dir
            "~/.eshell"))
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-prompt
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  :config
  ;; Consider eshell buffers real
  (defun +eshell-p (buf)
    (eq (buffer-local-value 'major-mode buf) 'eshell-mode))
  (add-to-list 'doom-real-buffer-functions #'+eshell-p nil #'eq)

  ;; Keep track of open eshell buffers
  (add-hook 'eshell-mode-hook #'+eshell|init)
  (add-hook 'eshell-exit-hook #'+eshell|cleanup)

  (after! em-term
    ;; Visual commands require a proper terminal. Eshell can't handle that, so
    ;; it delegates these commands to a term buffer.
    (setq eshell-visual-commands
          (append eshell-visual-commands '("tmux" "htop" "bash" "zsh" "fish" "vim" "nvim"))
          eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))))

  (defun +eshell|init-evil ()
    "Replace `evil-collection-eshell-next-prompt-on-insert' with
`+eshell|goto-prompt-on-insert'."
    (dolist (hook '(evil-replace-state-entry-hook evil-insert-state-entry-hook))
      (remove-hook hook 'evil-collection-eshell-next-prompt-on-insert t)
      (add-hook hook '+eshell|goto-prompt-on-insert nil t)))
  (add-hook 'eshell-mode-hook #'+eshell|init-evil)

  (defun +eshell|init-keymap ()
    "Setup eshell keybindings. This must be done in a hook because eshell-mode
redefines its keys every time `eshell-mode' is enabled."
    (map! :map eshell-mode-map
          :n [return]   #'+eshell/goto-end-of-prompt
          :n "c"        #'+eshell/evil-change
          :n "C"        #'+eshell/evil-change-line
          :n "d"        #'+eshell/evil-delete
          :n "D"        #'+eshell/evil-delete-line
          :i "C-d"      #'+eshell/quit-or-delete-char
          :i "C-p"      #'eshell-previous-input
          :i "C-n"      #'eshell-next-input
          [remap doom/backward-to-bol-or-indent] #'eshell-bol
          [remap doom/backward-kill-to-bol-and-indent] #'eshell-kill-input
          [remap split-window-below]  #'+eshell/split-below
          [remap split-window-right]  #'+eshell/split-right
          [remap evil-window-split]   #'+eshell/split-below
          [remap evil-window-vsplit]  #'+eshell/split-right))
  (add-hook 'eshell-first-time-mode-hook #'+eshell|init-keymap))

