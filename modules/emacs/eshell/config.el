;;; emacs/eshell/config.el -*- lexical-binding: t; -*-

;; see:
;;   + `+eshell/open': open in current buffer
;;   + `+eshell/open-popup': open in a popup
;;   + `+eshell/open-workspace': open in separate tab (requires :feature
;;     workspaces)

(defvar eshell-directory-name
  (let ((dir (expand-file-name "eshell" doom-private-dir)))
    (if (file-directory-p dir)
        dir
      "~/.eshell")))


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
        eshell-prompt-function #'+eshell-prompt
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  ;; Consider eshell buffers real
  (add-hook 'eshell-mode-hook #'doom|mark-buffer-as-real)

  ;; UI enhancements
  (defun +eshell|replace-fringes-with-margins ()
    "Remove eshell's fringes and give it a margin of 1."
    (set-window-fringes nil 0 0)
    (set-window-margins nil 1 1))
  (add-hook 'eshell-mode-hook #'+eshell|replace-fringes-with-margins)
  (add-hook 'eshell-mode-hook #'hide-mode-line-mode)

  ;; Keep track of open eshell buffers
  (add-hook 'eshell-mode-hook #'+eshell|init)
  (add-hook 'eshell-exit-hook #'+eshell|cleanup)

  (after! em-alias
    ;; Emulates popular shell utilities
    (map-put eshell-command-aliases-list "z" '("cd =$1"))
    (map-put eshell-command-aliases-list "bd" '("eshell-up $1")))

  (after! em-term
    ;; Visual commands require a proper terminal. Eshell can't handle that, so
    ;; it delegates these commands to a term buffer.
    (dolist (cmd '("tmux" "htop" "bash" "zsh" "fish" "vim" "nvim" "ncmpcpp"))
      (cl-pushnew cmd eshell-visual-commands)))

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
    (when (featurep 'evil)
      (evil-define-key* 'normal eshell-mode-map
        [return]   #'+eshell/goto-end-of-prompt
        "c"        #'+eshell/evil-change
        "C"        #'+eshell/evil-change-line
        "d"        #'+eshell/evil-delete
        "D"        #'+eshell/evil-delete-line)
      (evil-define-key* 'insert eshell-mode-map
        [tab]      #'+eshell/pcomplete
        "\C-d"     #'+eshell/quit-or-delete-char
        "\C-p"     #'eshell-previous-input
        "\C-n"     #'eshell-next-input))
    (define-key! eshell-mode-map
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
