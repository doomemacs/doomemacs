;;; emacs/eshell/config.el -*- lexical-binding: t; -*-

;; see:
;;   + `+eshell/open': open in current buffer
;;   + `+eshell/open-popup': open in a popup
;;   + `+eshell/open-workspace': open in separate tab (requires :feature
;;     workspaces)

(defvar eshell-directory-name (concat doom-etc-dir "eshell"))

(defvar eshell-aliases-file
  (expand-file-name "eshell_aliases" doom-private-dir)
  "The path to your eshell aliases file, where you may declare alises. This is
here as an alternative to `set-eshell-alias!'.")

;;
(defvar +eshell-enable-new-shell-on-split t
  "If non-nil, spawn a new eshell session after splitting from an eshell
buffer.")

(defvar +eshell-kill-window-on-exit nil
  "If non-nil, eshell will close windows along with its eshell buffers.")

(defvar +eshell-aliases
  '(("q"  "exit")           ; built-in
    ("z"  "cd =$1")         ; built-in
    ("bd" "eshell-up $1")   ; `eshell-up'
    ("rg" "rg --color=always")
    ("ag" "ag --color=always"))
  "An alist of default eshell aliases, meant to emulate useful shell utilities,
like fasd and bd. Note that you may overwrite these in your
`eshell-aliases-file'. This is here to provide an alternative, elisp-centric way
to define your aliases.

You should use `det-eshell-alias!' to change this.")

(defvar +eshell--default-aliases nil)


;;
;; Plugins
;;

(after! eshell ; built-in
  (setq eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face))
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-buffer-shorthand t
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-default-prompt
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  ;; Consider eshell buffers real
  (add-hook 'eshell-mode-hook #'doom|mark-buffer-as-real)

  ;; Keep track of open eshell buffers
  (add-hook 'eshell-mode-hook #'+eshell|init)
  (add-hook 'eshell-exit-hook #'+eshell|cleanup)

  ;; UI enhancements
  (defun +eshell|replace-fringes-with-margins ()
    "Remove eshell's fringes and give it a margin of 1."
    (set-window-fringes nil 0 0)
    (set-window-margins nil 1 1))
  (add-hook 'eshell-mode-hook #'+eshell|replace-fringes-with-margins)
  (add-hook 'eshell-mode-hook #'hide-mode-line-mode)

  ;; Don't auto-write our aliases! Let us manage our own `eshell-aliases-file'
  ;; or configure `+eshell-aliases' via elisp.
  (advice-add #'eshell-write-aliases-list :override #'ignore)

  ;; Visual commands require a proper terminal. Eshell can't handle that, so
  ;; it delegates these commands to a term buffer.
  (after! em-term
    (dolist (cmd '("tmux" "htop" "bash" "zsh" "fish" "vim" "nvim" "ncmpcpp"))
      (add-to-list 'eshell-visual-commands cmd)))

  (defun +eshell|init-aliases ()
    (setq +eshell--default-aliases eshell-command-aliases-list
          eshell-command-aliases-list
          (append eshell-command-aliases-list
                  +eshell-aliases)))
  (add-hook 'eshell-alias-load-hook #'+eshell|init-aliases)

  (when (featurep! :feature evil +everywhere)
    (add-hook 'eshell-mode-hook #'+eshell|init-evil))

  (defun +eshell|init-keymap ()
    "Setup eshell keybindings. This must be done in a hook because eshell-mode
redefines its keys every time `eshell-mode' is enabled."
    (when (featurep 'evil)
      (evil-define-key* 'normal eshell-mode-map
        [return]   #'+eshell/goto-end-of-prompt
        "c"        #'+eshell/evil-change
        "C"        #'+eshell/evil-change-line
        "d"        #'+eshell/evil-delete
        "D"        #'+eshell/evil-delete-line)
      (evil-define-key* 'insert eshell-mode-map
        [tab]      #'+eshell/pcomplete
        "\C-j"     #'evil-window-down
        "\C-k"     #'evil-window-up
        "\C-h"     #'evil-window-left
        "\C-l"     #'evil-window-right
        "\C-d"     #'+eshell/quit-or-delete-char
        "\C-p"     #'eshell-previous-input
        "\C-n"     #'eshell-next-input))
    (define-key! eshell-mode-map
      (kbd "C-s")   #'+eshell/search-history
      (kbd "C-c s") #'+eshell/split-below
      (kbd "C-c v") #'+eshell/split-right
      (kbd "C-c x") #'+eshell/kill-and-close
      [remap split-window-below]  #'+eshell/split-below
      [remap split-window-right]  #'+eshell/split-right
      [remap doom/backward-to-bol-or-indent] #'eshell-bol
      [remap doom/backward-kill-to-bol-and-indent] #'eshell-kill-input
      [remap evil-window-split]   #'+eshell/split-below
      [remap evil-window-vsplit]  #'+eshell/split-right))
  (add-hook 'eshell-first-time-mode-hook #'+eshell|init-keymap))


(def-package! eshell-up
  :commands (eshell-up eshell-up-peek))


(def-package! shrink-path
  :commands shrink-path-file)
