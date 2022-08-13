;;; term/eshell/config.el -*- lexical-binding: t; -*-

;; see:
;;   + `+eshell/here': open eshell in the current window
;;   + `+eshell/toggle': toggles an eshell popup
;;   + `+eshell/frame': converts the current frame into an eshell-dedicated
;;   frame. Once the last eshell process is killed, the old frame configuration
;;   is restored.

(defvar +eshell-config-dir
  (expand-file-name "eshell/" doom-user-dir)
  "Where to store eshell configuration files, as opposed to
`eshell-directory-name', which is where Doom will store temporary/data files.")

(defvar eshell-directory-name (concat doom-data-dir "eshell")
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
    ("ff" "find-file-other-window $1")
    ("d"  "dired $1")
    ("bd" "eshell-up $1")
    ("rg" "rg --color=always $*")
    ("l"  "ls -lh $*")
    ("ll" "ls -lah $*")
    ("git" "git --no-pager $*")
    ("gg" "magit-status")
    ("cdp" "cd-to-project")
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
  (when (modulep! :ui workspaces)
    (add-hook 'persp-activated-functions #'+eshell-switch-workspace-fn)
    (add-hook 'persp-before-switch-functions #'+eshell-save-workspace-fn))

  ;; UI enhancements
  (add-hook! 'eshell-mode-hook
    (defun +eshell-remove-fringes-h ()
      (set-window-fringes nil 0 0)
      (set-window-margins nil 1 nil))
    (defun +eshell-enable-text-wrapping-h ()
      (visual-line-mode +1)
      (set-display-table-slot standard-display-table 0 ?\ )))

  (add-hook 'eshell-mode-hook #'hide-mode-line-mode)

  ;; Remove hscroll-margin in shells, otherwise you get jumpiness when the
  ;; cursor comes close to the left/right edges of the window.
  (setq-hook! 'eshell-mode-hook hscroll-margin 0)

  ;; Don't auto-write our aliases! Let us manage our own `eshell-aliases-file'
  ;; or configure `+eshell-aliases' via elisp.
  (advice-add #'eshell-write-aliases-list :override #'ignore)

  ;; REVIEW In Emacs 27 and newer, waiting for esh-module is unnecessary.
  (after! esh-module
    (add-to-list 'eshell-modules-list 'eshell-tramp))

  ;; Visual commands require a proper terminal. Eshell can't handle that, so
  ;; it delegates these commands to a term buffer.
  (after! em-term
    (pushnew! eshell-visual-commands "tmux" "htop" "vim" "nvim" "ncmpcpp"))

  (after! em-alias
    (setq +eshell--default-aliases eshell-command-aliases-list
          eshell-command-aliases-list
          (append eshell-command-aliases-list
                  +eshell-aliases)))

  (set-lookup-handlers! 'eshell-mode :documentation #'+eshell-help-run-help)

  (map! :map Man-mode-map :n "x" #'+eshell-man-to-tldr)
  (map! :map tldr-mode-map :n "x" #'+eshell-tldr-to-man)

  (defvar +eshell-matches-in-history)
  (defvar +eshell-history-current-match)

  (defadvice! +eshell-previous-matching-input-from-input-a (arg)
    "Like `eshell-previous-matching-input-from-input'.
But uses `completion-styles' to find the mathces."
    :override #'eshell-previous-matching-input-from-input
    (interactive "p")
    (when (not (memq last-command '(eshell-previous-matching-input-from-input
                                    eshell-next-matching-input-from-input)))
      ;; Starting a new search
      (setq +eshell-history-current-match 0
            eshell-matching-input-from-input-string
            (buffer-substring (save-excursion (eshell-bol) (point))
                              (point))
            +eshell-matches-in-history
            (completion-all-completions eshell-matching-input-from-input-string
                                        (ring-elements eshell-history-ring)
                                        nil 0))
      (setf (nthcdr (safe-length +eshell-matches-in-history)
                    +eshell-matches-in-history)
            nil)
      (cl-callf (lambda (list) (ring-convert-sequence-to-ring
                           (cons eshell-matching-input-from-input-string
                                 (delete-dups list))))
          +eshell-matches-in-history))
    (if (ring-empty-p +eshell-matches-in-history)
        (message "No matches in history for %s"
                 eshell-matching-input-from-input-string)
      (if (eq 0 arg)
          (setq +eshell-history-current-match 0)
        (cl-callf + +eshell-history-current-match arg))
      (delete-region eshell-last-output-end (point))
      (insert (ring-ref +eshell-matches-in-history
                        +eshell-history-current-match)))))

(after! esh-mode
  (map! :map eshell-mode-map
        :n  "RET"    #'+eshell/goto-end-of-prompt
        :n  [return] #'+eshell/goto-end-of-prompt
        :ni "C-j"    #'eshell-next-matching-input-from-input
        :ni "C-k"    #'eshell-previous-matching-input-from-input
        :ig "C-d"    #'+eshell/quit-or-delete-char
        :i  "C-c h"  #'evil-window-left
        :i  "C-c j"  #'evil-window-down
        :i  "C-c k"  #'evil-window-up
        :i  "C-c l"  #'evil-window-right
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
        ;; To emulate terminal keybinds
        "C-l"   (cmd! (eshell/clear-scrollback) (eshell-emit-prompt))
        (:localleader
         "b" #'eshell-insert-buffer-name
         "e" #'eshell-insert-envvar
         "s" #'+eshell/search-history)))


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
  :config
  (setup-esh-help-eldoc)
  ;; Man can choke on some paths, like ~/dir/some-exe
  (defadvice! +eshell-eldoc-function-a (func cmd)
    "Don't try to parse man output unless a manpage exists."
    :around #'esh-help-eldoc-man-minibuffer-string
    (if-let ((cache-result (gethash cmd esh-help-man-cache)))
        (unless (eql 'none cache-result)
          cache-result)
      (if (Man-completion-table cmd nil nil)
          (funcall func cmd)
        (prog1 nil
          (puthash cmd 'none esh-help-man-cache))))))


(use-package! eshell-did-you-mean
  :after esh-mode ; Specifically esh-mode, not eshell
  :config
  (eshell-did-you-mean-setup)
  ;; HACK There is a known issue with `eshell-did-you-mean' where it does not
  ;;      work on first invocation, so we invoke it once manually by setting the
  ;;      last command and then calling the output filter.
  (setq eshell-last-command-name "catt")
  (eshell-did-you-mean-output-filter "catt: command not found"))


(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode)
  :config
  (defadvice! +eshell-filter-history-from-highlighting-a (&rest _)
    "Selectively inhibit `eshell-syntax-highlighting-mode'.
So that mathces from history show up with highlighting."
    :before-until #'eshell-syntax-highlighting--enable-highlighting
    (memq this-command '(eshell-previous-matching-input-from-input
                         eshell-next-matching-input-from-input)))

  (defun +eshell-syntax-highlight-maybe-h ()
    "Hook added to `pre-command-hook' to restore syntax highlighting
when inhibited to show history matches."
    (when (and eshell-syntax-highlighting-mode
               (memq last-command '(eshell-previous-matching-input-from-input
                                    eshell-next-matching-input-from-input)))
      (eshell-syntax-highlighting--enable-highlighting)))

  (defun +eshell-syntax-highlighting-mode-h ()
    "Hook to enable `+eshell-syntax-highlight-maybe-h'."
    (if eshell-syntax-highlighting-mode
        (add-hook 'pre-command-hook #'+eshell-syntax-highlight-maybe-h nil t)
      (remove-hook 'pre-command-hook #'+eshell-syntax-highlight-maybe-h t)))

  (add-hook 'eshell-syntax-highlighting-mode-hook #'+eshell-syntax-highlighting-mode-h))


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
    (unless (executable-find "fish") ""))

  (when (modulep! :completion vertico)
    ;; Code is mostly from  https://github.com/minad/marginalia/issues/87
    ;; But we implement a capf because getting annotations from fish is
    ;; difficult if we stick with pcomplete. The capf is non-exclusive
    ;; so fallback to pcomplete machinery happens if there are no candidates.
    (defun +eshell-fish-completion-list (raw-prompt)
      "Return list of completion candidates for RAW-PROMPT."
      (mapcar (lambda (e) (let ((res (split-string e "\t")))
                       (propertize (car res) 'fish-annotation (cadr res))))
              (split-string
               (fish-completion--list-completions-with-desc raw-prompt)
               "\n" t)))

    (defun +eshell-fish-capf ()
      "A a capf for fish-completion."
      (when-let ((args (ignore-errors (eshell-complete-parse-arguments)))
                 (table (+eshell-fish-completion-list
                         (buffer-substring (cadr args) (point))))
                 ((not (file-exists-p (car table)))))
        (list (car (last args)) (point) table
              :exclusive 'no
              :annotation-function #'+eshell-fish-completion-annotate
              :exit-function (lambda (&rest _) (insert " ")))))

    (defun +eshell-fish-completion-annotate (cand)
      (when-let* ((ann (get-text-property 0 'fish-annotation cand)))
        (concat (propertize " " 'display '(space :align-to center)) ann)))

    (defun +eshell-use-annotated-completions-h ()
      "Use annotaed fish completions."
      (if fish-completion-mode
          (add-hook 'completion-at-point-functions #'+eshell-fish-capf nil t)
        (remove-hook 'completion-at-point-functions #'+eshell-fish-capf t)))

    (add-hook 'fish-completion-mode-hook #'+eshell-use-annotated-completions-h)))

(when (>= emacs-major-version 28)
;; Why is this delayed evaluation needed? It should'nt be according to the docs
;; `custom-set-faces!' but it doesn't works for me without it.
  (after! esh-mode
    (let ((ansi-color-set
           (lambda (color &optional name)
             (custom-set-faces! `(,(intern-soft
                                    (concat "ansi-color-"
                                            (or name (symbol-name color))))
                                  :foreground ,(doom-color color)
                                  :background ,(doom-color color))))))
      (mapc ansi-color-set '(red blue green yellow magenta cyan))
      (mapc (lambda (arg) (apply ansi-color-set arg))
            '((base0 "white") (base2 "bright-white")
              (base8 "black") (base5 "bright-black")
              (orange "bright-red") (teal "bright-green")
              (yellow "bright-yellow") (blue "bright-blue")
              (violet "bright-magenta") (dark-cyan "bright-cyan"))))))

(after! tldr (set-popup-rule! "^\\*tldr\\*" :side 'bottom :size 0.45))
