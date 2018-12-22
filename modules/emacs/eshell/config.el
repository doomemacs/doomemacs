;;; emacs/eshell/config.el -*- lexical-binding: t; -*-

;; see:
;;   + `+eshell/open': open in current buffer
;;   + `+eshell/open-popup': open in a popup
;;   + `+eshell/open-fullscreen': open eshell fullscreen (will restore window
;;     config when quitting the last eshell buffer)

(defvar +eshell-config-dir
  (expand-file-name "eshell/" doom-private-dir)
  "Where to store eshell configuration files, as opposed to
`eshell-directory-name', which is where Doom will store temporary/data files.")

(defvar +eshell-enable-new-shell-on-split t
  "If non-nil, spawn a new eshell session after splitting from an eshell
buffer.")

(defvar +eshell-kill-window-on-exit nil
  "If non-nil, eshell will close windows along with its eshell buffers.")

(defvar +eshell-aliases
  '(("q"  "exit")           ; built-in
    ("f"  "find-file $1")
    ("bd" "eshell-up $1")   ; `eshell-up'
    ("rg" "rg --color=always")
    ("ag" "ag --color=always")
    ("l"  "ls -lh")
    ("ll" "ls -lah")
    ("clear" "clear-scrollback")) ; more sensible than default
  "An alist of default eshell aliases, meant to emulate useful shell utilities,
like fasd and bd. Note that you may overwrite these in your
`eshell-aliases-file'. This is here to provide an alternative, elisp-centric way
to define your aliases.

You should use `det-eshell-alias!' to change this.")

;;
(defvar eshell-directory-name (concat doom-etc-dir "eshell"))

;; These files are exceptions, because they may contain configuration
(defvar eshell-aliases-file (concat +eshell-config-dir "alias"))
(defvar eshell-rc-script    (concat +eshell-config-dir "profile"))
(defvar eshell-login-script (concat +eshell-config-dir "login"))


(defvar +eshell--default-aliases nil)


;;
;; Packages

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
        ;; don't record command in history if prefixed with whitespace
        eshell-input-filter #'eshell-input-filter-initial-space
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-default-prompt
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  (add-to-list 'doom-detect-indentation-excluded-modes 'eshell-mode nil #'eq)

  ;; Consider eshell buffers real
  (add-hook 'eshell-mode-hook #'doom|mark-buffer-as-real)

  ;; Keep track of open eshell buffers
  (add-hook 'eshell-mode-hook #'+eshell|init)
  (add-hook 'eshell-exit-hook #'+eshell|cleanup)

  ;; Enable autopairing in eshell
  (add-hook 'eshell-mode-hook #'smartparens-mode)

  ;; Persp-mode/workspaces integration
  (when (featurep! :feature workspaces)
    (add-hook 'persp-activated-functions #'+eshell|switch-workspace)
    (add-hook 'persp-before-switch-functions #'+eshell|save-workspace))

  ;; UI enhancements
  (defun +eshell|remove-fringes ()
    (set-window-fringes nil 0 0)
    (set-window-margins nil 1 nil))
  (add-hook 'eshell-mode-hook #'+eshell|remove-fringes)

  (defun +eshell|enable-text-wrapping ()
    (visual-line-mode +1)
    (set-display-table-slot standard-display-table 0 ?\ ))
  (add-hook 'eshell-mode-hook #'+eshell|enable-text-wrapping)

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
    (map! :map eshell-mode-map
          :n [return]   #'+eshell/goto-end-of-prompt
          :n "c"        #'+eshell/evil-change
          :n "C"        #'+eshell/evil-change-line
          :n "d"        #'+eshell/evil-delete
          :n "D"        #'+eshell/evil-delete-line
          :i [tab]      #'+eshell/pcomplete
          :i "C-j"     #'evil-window-down
          :i "C-k"     #'evil-window-up
          :i "C-h"     #'evil-window-left
          :i "C-l"     #'evil-window-right
          :i "C-d"     #'+eshell/quit-or-delete-char
          :i "C-p"     #'eshell-previous-input
          :i "C-n"     #'eshell-next-input
          "C-s"   #'+eshell/search-history
          "C-c s" #'+eshell/split-below
          "C-c v" #'+eshell/split-right
          "C-c x" #'+eshell/kill-and-close
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


(def-package! eshell-z
  :after eshell
  :config
  ;; Use zsh's db if it exists, otherwise, store it in `doom-cache-dir'
  (unless (file-exists-p eshell-z-freq-dir-hash-table-file-name)
    (setq eshell-z-freq-dir-hash-table-file-name
          (expand-file-name "z" eshell-directory-name))))
