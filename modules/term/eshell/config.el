;;; term/eshell/config.el -*- lexical-binding: t; -*-

;; see:
;;   + `+eshell/here': open eshell in the current window
;;   + `+eshell/toggle': toggles an eshell popup
;;   + `+eshell/frame': converts the current frame into an eshell-dedicated
;;   frame. Once the last eshell process is killed, the old frame configuration
;;   is restored.

(defvar +eshell-config-dir
  (expand-file-name "eshell/" doom-private-dir)
  "Where to store eshell configuration files, as opposed to
`eshell-directory-name', which is where Doom will store temporary/data files.")

(defvar eshell-directory-name (concat doom-etc-dir "eshell")
  "Where to store temporary/data files, as opposed to `eshell-config-dir',
which is where Doom will store eshell configuration files.")

(defvar +eshell-enable-new-shell-on-split t
  "If non-nil, spawn a new eshell session after splitting from an eshell
buffer.")

(defvar +eshell-kill-window-on-exit nil
  "If non-nil, eshell will close windows along with its eshell buffers.")

(defvar +eshell-aliases
  '(("q"  "exit")           ; built-in
    ("f"  "find-file $1")
    ("ff" "find-file $1")
    ("d"  "dired $1")
    ("bd" "eshell-up $1")
    ("rg" "rg --color=always $*")
    ("l"  "ls -lh $*")
    ("ll" "ls -lah $*")
    ("gg" "magit-status")
    ("clear" "clear-scrollback")) ; more sensible than default
  "An alist of default eshell aliases, meant to emulate useful shell utilities,
like fasd and bd. Note that you may overwrite these in your
`eshell-aliases-file'. This is here to provide an alternative, elisp-centric way
to define your aliases.

You should use `set-eshell-alias!' to change this.")

;; These files are exceptions, because they may contain configuration
(defvar eshell-aliases-file (concat +eshell-config-dir "aliases"))
(defvar eshell-rc-script    (concat +eshell-config-dir "profile"))
(defvar eshell-login-script (concat +eshell-config-dir "login"))

(defvar +eshell--default-aliases nil)


;;
;;; Packages

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
        ;; TODO Use `eshell-input-filter-initial-space' when Emacs 25 support is dropped
        eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input)))
        ;; em-prompt
        eshell-prompt-regexp "^.* λ "
        eshell-prompt-function #'+eshell-default-prompt-fn
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  ;; Consider eshell buffers real
  (add-hook 'eshell-mode-hook #'doom-mark-buffer-as-real-h)

  ;; Keep track of open eshell buffers
  (add-hook 'eshell-mode-hook #'+eshell-init-h)
  (add-hook 'eshell-exit-hook #'+eshell-cleanup-h)

  ;; Enable autopairing in eshell
  (add-hook 'eshell-mode-hook #'smartparens-mode)

  ;; Persp-mode/workspaces integration
  (when (featurep! :ui workspaces)
    (add-hook 'persp-activated-functions #'+eshell-switch-workspace-fn)
    (add-hook 'persp-before-switch-functions #'+eshell-save-workspace-fn))

  ;; UI enhancements
  (add-hook! 'eshell-mode-hook
    (defun +eshell-remove-fringes-h ()
      (set-window-fringes nil 0 0)
      (set-window-margins nil 1 nil)))

  (add-hook! 'eshell-mode-hook
    (defun +eshell-enable-text-wrapping-h ()
      (visual-line-mode +1)
      (set-display-table-slot standard-display-table 0 ?\ )))

  (add-hook 'eshell-mode-hook #'hide-mode-line-mode)

  ;; Don't auto-write our aliases! Let us manage our own `eshell-aliases-file'
  ;; or configure `+eshell-aliases' via elisp.
  (advice-add #'eshell-write-aliases-list :override #'ignore)

  ;; Visual commands require a proper terminal. Eshell can't handle that, so
  ;; it delegates these commands to a term buffer.
  (after! em-term
    (pushnew! eshell-visual-commands "tmux" "htop" "vim" "nvim" "ncmpcpp"))

  (add-hook! 'eshell-alias-load-hook
    (defun +eshell-init-aliases-h ()
      (setq +eshell--default-aliases eshell-command-aliases-list
            eshell-command-aliases-list
            (append eshell-command-aliases-list
                    +eshell-aliases))))

  (add-hook! 'eshell-first-time-mode-hook
    (defun +eshell-init-keymap-h ()
      ;; Keys must be bound in a hook because eshell resets its keymap every
      ;; time `eshell-mode' is enabled. Why? It is not for us mere mortals to
      ;; grasp such wisdom.
      (map! :map eshell-mode-map
            :n "RET"     #'+eshell/goto-end-of-prompt
            :n [return]  #'+eshell/goto-end-of-prompt
            :ni "C-j"    #'eshell-next-matching-input-from-input
            :ni "C-k"    #'eshell-previous-matching-input-from-input
            :ig "C-d"    #'+eshell/quit-or-delete-char
            :i "C-c h"   #'evil-window-left
            :i "C-c j"   #'evil-window-down
            :i "C-c k"   #'evil-window-up
            :i "C-c l"   #'evil-window-right
            "C-s"   #'+eshell/search-history
            ;; Emacs bindings
            "C-e"   #'end-of-line
            ;; Tmux-esque prefix keybinds
            "C-c s" #'+eshell/split-below
            "C-c v" #'+eshell/split-right
            "C-c x" #'+eshell/kill-and-close
            [remap split-window-below]  #'+eshell/split-below
            [remap split-window-right]  #'+eshell/split-right
            [remap doom/backward-to-bol-or-indent] #'eshell-bol
            [remap doom/backward-kill-to-bol-and-indent] #'eshell-kill-input
            [remap evil-delete-back-to-indentation] #'eshell-kill-input
            [remap evil-window-split]   #'+eshell/split-below
            [remap evil-window-vsplit]  #'+eshell/split-right
            (:localleader
             "b" #'eshell-insert-buffer-name
             "e" #'eshell-insert-envvar
             "s" #'+eshell/search-history)))))


(use-package! eshell-up
  :commands eshell-up eshell-up-peek)


(use-package! eshell-z
  :after eshell
  :config
  ;; Use zsh's db if it exists, otherwise, store it in `doom-cache-dir'
  (unless (file-exists-p eshell-z-freq-dir-hash-table-file-name)
    (setq eshell-z-freq-dir-hash-table-file-name
          (expand-file-name "z" eshell-directory-name))))


(use-package! esh-help
  :after eshell
  :config (setup-esh-help-eldoc))


(use-package! eshell-did-you-mean
  :after esh-mode ; Specifically esh-mode, not eshell
  :config
  (eshell-did-you-mean-setup)
  ;; HACK There is a known issue with `eshell-did-you-mean' where it does not
  ;;      work on first invocation, so we invoke it once manually by setting the
  ;;      last command and then calling the output filter.
  (setq eshell-last-command-name "catt")
  (eshell-did-you-mean-output-filter "catt: command not found"))


(use-package! fish-completion
  :unless IS-WINDOWS
  :hook (eshell-mode . fish-completion-mode)
  :init (setq fish-completion-fallback-on-bash-p t)
  :config
  ;; HACK Even with `fish-completion-fallback-on-bash-p' non-nil,
  ;;      `fish-completion--list-completions-with-desc' will throw an error if
  ;;      fish isn't installed (and so, will fail to fall back to bash), so we
  ;;      advise it to fail silently.
  (defadvice! +eshell--fallback-to-bash-a (&rest _)
    :before-until #'fish-completion--list-completions-with-desc
    (unless (executable-find "fish") "")))
