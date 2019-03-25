;;; config/default/config.el -*- lexical-binding: t; -*-

(defvar +default-minibuffer-maps
  `(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map
    ,@(if (featurep! :completion ivy) '(ivy-minibuffer-map)))
  "A list of all the keymaps used for the minibuffer.")


;;
;;; Reasonable defaults

(after! epa
  (setq epa-file-encrypt-to
        (or epa-file-encrypt-to
            ;; Collect all public key IDs with your username
            (unless (string-empty-p user-full-name)
              (cl-loop for key in (ignore-errors (epg-list-keys (epg-make-context) user-full-name))
                       collect (epg-sub-key-id (car (epg-key-sub-key-list key)))))
            user-mail-address)
        ;; With GPG 2.1, this forces gpg-agent to use the Emacs minibuffer to
        ;; prompt for the key passphrase.
        epa-pinentry-mode 'loopback))


;;
;;; Keybinding fixes

;; This section is dedicated to "fixing" certain keys so that they behave
;; sensibly (and consistently with similar contexts).

;; Consistently use q to quit windows
(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))

;; OS specific fixes
(when IS-MAC
  ;; Fix MacOS shift+tab
  (define-key input-decode-map [S-iso-lefttab] [backtab])
  ;; Fix conventional OS keys in Emacs
  (map! "s-`" #'other-frame  ; fix frame-switching
        ;; fix OS window/frame navigation/manipulation keys
        "s-w" #'delete-window
        "s-W" #'delete-frame
        "s-n" #'+default/new-buffer
        "s-N" #'make-frame
        "s-q" (if (daemonp) #'delete-frame #'save-buffers-kill-terminal)
        "C-s-f" #'toggle-frame-fullscreen
        ;; Restore somewhat common navigation
        "s-l" #'goto-line
        ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
        ;; it imposes some other functionality and overhead we don't need)
        "s-f" #'swiper
        "s-z" #'undo
        "s-Z" #'redo
        "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
        "s-v" #'yank
        "s-s" #'save-buffer
        ;; Buffer-local font scaling
        "s-+" (λ! (text-scale-set 0))
        "s-=" #'text-scale-increase
        "s--" #'text-scale-decrease
        ;; Conventional text-editing keys & motions
        "s-a" #'mark-whole-buffer
        :g "s-/" (λ! (save-excursion (comment-line 1)))
        :n "s-/" #'evil-commentary-line
        :v "s-/" #'evil-commentary
        :gni [s-return]    #'+default/newline-below
        :gni [S-s-return]  #'+default/newline-above
        :gi  [s-backspace] #'doom/backward-kill-to-bol-and-indent
        :gi  [s-left]      #'doom/backward-to-bol-or-indent
        :gi  [s-right]     #'doom/forward-to-last-non-comment-or-eol
        :gi  [M-backspace] #'backward-kill-word
        :gi  [M-left]      #'backward-word
        :gi  [M-right]     #'forward-word))


;;
;;; Keybind schemes

;; Custom help keys -- these aren't under `+bindings' because they ought to be
;; universal.
(map! :map help-map
      "'"   #'describe-char
      "a"   #'apropos ; replaces `apropos-command'
      "A"   #'doom/describe-autodefs
      "B"   #'doom/open-bug-report
      "C-c" #'describe-coding-system ; replaces `describe-copying' b/c not useful
      "d"   #'doom/describe-module ; replaces `apropos-documentation' b/c `apropos' covers this
      "D"   #'doom/open-manual
      "E"   #'doom/open-vanilla-sandbox
      "F"   #'describe-face ; replaces `Info-got-emacs-command-node' b/c redundant w/ `Info-goto-node'
      "h"   #'doom/describe-symbol ; replaces `view-hello-file' b/c annoying
      "C-k" #'describe-key-briefly
      "L"   #'global-command-log-mode ; replaces `describe-language-environment' b/c remapped to C-l
      "C-l" #'describe-language-environment
      "M"   #'doom/describe-active-minor-mode
      "C-m" #'info-emacs-manual
      "n"   #'doom/open-news ; replaces `view-emacs-news' b/c it's on C-n too
      "O"   #'+lookup/online
      "p"   #'doom/describe-package ; replaces `finder-by-keyword'
      "P"   #'find-library ; replaces `describe-package' b/c redundant w/ `doom/describe-package'
      "r" nil ; replaces `info-emacs-manual' b/c it's on C-m now
      (:prefix "r"
        "r"   #'doom/reload
        "t"   #'doom/reload-theme
        "p"   #'doom/reload-packages
        "f"   #'doom/reload-font
        "P"   #'doom/reload-project)
      "T"   #'doom/toggle-profiler
      "V"   #'set-variable
      "C-v" #'doom/version
      "W"   #'+default/man-or-woman)

(after! which-key
  (which-key-add-key-based-replacements "C-h r" "reload")
  (when (featurep 'evil)
    (which-key-add-key-based-replacements (concat doom-leader-key     " r") "reload")
    (which-key-add-key-based-replacements (concat doom-leader-alt-key " r") "reload")))


(when (featurep! +bindings)
  ;; Make M-x harder to miss
  (define-key! 'override
    "M-x" #'execute-extended-command
    "A-x" #'execute-extended-command)

  ;; A Doom convention where C-s on popups and interactive searches will invoke
  ;; ivy/helm for their superior filtering.
  (define-key! :keymaps +default-minibuffer-maps
    "C-s"    (if (featurep! :completion ivy)
                 #'counsel-minibuffer-history
               #'helm-minibuffer-history))

  ;; Smarter C-a/C-e for both Emacs and Evil. C-a will jump to indentation.
  ;; Pressing it again will send you to the true bol. Same goes for C-e, except
  ;; it will ignore comments+trailing whitespace before jumping to eol.
  (map! :gi "C-a" #'doom/backward-to-bol-or-indent
        :gi "C-e" #'doom/forward-to-last-non-comment-or-eol
        ;; Standardize the behavior of M-RET/M-S-RET as a "add new item
        ;; below/above" key.
        :gni [M-return]    #'+default/newline-below
        :gni [M-S-return]  #'+default/newline-above
        :gni [C-return]    #'+default/newline-below
        :gni [C-S-return]  #'+default/newline-above))


;;
;;; Bootstrap configs

(if (featurep 'evil)
    (load! "+evil")
  (load! "+emacs"))
