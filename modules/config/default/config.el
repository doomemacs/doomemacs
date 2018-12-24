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
;; Reasonable defaults

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


(when (featurep! +smartparens)
  ;; disable :unless predicates with (sp-pair "'" nil :unless nil)
  ;; disable :post-handlers with (sp-pair "{" nil :post-handlers nil)
  ;; ...or specific :post-handlers with (sp-pair "{" nil :post-handlers '(:rem
  ;; ("| " "SPC")))
  (after! smartparens
    ;; Autopair quotes more conservatively; if I'm next to a word/before another
    ;; quote, I likely don't want another pair.
    (let ((unless-list '(sp-point-before-word-p
                         sp-point-after-word-p
                         sp-point-before-same-p)))
      (sp-pair "'"  nil :unless unless-list)
      (sp-pair "\"" nil :unless unless-list))

    ;; Expand {|} => { | }
    ;; Expand {|} => {
    ;;   |
    ;; }
    (dolist (brace '("(" "{" "["))
      (sp-pair brace nil
               :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
               ;; I likely don't want a new pair if adjacent to a word or opening brace
               :unless '(sp-point-before-word-p sp-point-before-same-p)))

    ;; Major-mode specific fixes
    (sp-local-pair '(ruby-mode enh-ruby-mode) "{" "}"
                   :pre-handlers '(:rem sp-ruby-pre-handler)
                   :post-handlers '(:rem sp-ruby-post-handler))

    ;; Don't do square-bracket space-expansion where it doesn't make sense to
    (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                   "[" nil :post-handlers '(:rem ("| " "SPC")))

    ;; Reasonable default pairs for comments
    (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
                   "<!--" "-->" :actions '(insert) :post-handlers '(("| " "SPC")))

    (sp-local-pair
     '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
                java-mode php-mode css-mode scss-mode less-css-mode stylus-mode)
     "/*" "*/"
     :actions '(insert)
     :post-handlers '(("| " "SPC") ("|\n*/[i][d-2]" "RET") ("\n* ||\n*/[i][d-2]" "*")))

    ;; Highjacks backspace to:
    ;;  a) balance spaces inside brackets/parentheses ( | ) -> (|)
    ;;  b) delete space-indented `tab-width' steps at a time
    ;;  c) close empty multiline brace blocks in one step:
    ;;     {
    ;;     |
    ;;     }
    ;;     becomes {|}
    ;;  d) refresh smartparens' :post-handlers, so SPC and RET expansions work
    ;;     even after a backspace.
    ;;  e) properly delete smartparen pairs when they are encountered, without
    ;;     the need for strict mode.
    ;;  f) do none of this when inside a string
    (advice-add #'delete-backward-char :override #'doom/delete-backward-char)

    ;; Makes `newline-and-indent' smarter when dealing with comments
    (advice-add #'newline-and-indent :around #'doom*newline-indent-and-continue-comments)))


;;
;; Keybinding fixes

;; This section is dedicated to "fixing" certain keys so that they behave
;; sensibly (and consistently with similar contexts).

;; Make SPC u SPC u [...] possible (#747)
(map! :map universal-argument-map
      :prefix doom-leader-key     "u" #'universal-argument-more
      :prefix doom-leader-alt-key "u" #'universal-argument-more)

(defun +default|setup-input-decode-map ()
  "Ensure TAB and [tab] are treated the same in TTY Emacs."
  (define-key input-decode-map (kbd "TAB") [tab]))
(add-hook 'tty-setup-hook #'+default|setup-input-decode-map)

;; Restore CUA keys in minibuffer
(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit
  "C-v"    #'yank
  "C-z"    (λ! (ignore-errors (call-interactively #'undo)))
  "C-a"    #'move-beginning-of-line
  "C-b"    #'backward-word
  ;; A Doom convention where C-s on popups and interactive searches will invoke
  ;; ivy/helm for their superior filtering.
  "C-s"    (if (featurep! :completion ivy)
               #'counsel-minibuffer-history
             #'helm-minibuffer-history))

;; Consistently use q to quit windows
(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))

;; OS specific fixes
(when IS-MAC
  ;; Fix MacOS shift+tab
  (define-key input-decode-map [S-iso-lefttab] [backtab])

  (define-key!
    ;; Buffer-local font scaling
    "s-+" (λ! (text-scale-set 0))
    "s-=" #'text-scale-increase
    "s--" #'text-scale-decrease
    ;; Fix frame-switching on MacOS
    "s-`" #'other-frame
    ;; Simple window/frame navigation/manipulation
    "s-w" #'delete-window
    "s-W" #'delete-frame
    "s-n" #'+default/new-buffer
    "s-N" #'make-frame
    ;; Textmate-esque bindings
    "s-a" #'mark-whole-buffer
    "s-b" #'+default/compile
    "s-f" #'swiper
    "s-q" (if (daemonp) #'delete-frame #'evil-quit-all)
    ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
    ;; it imposes some other functionality and overhead we don't need)
    "s-z" #'undo
    "s-s" #'save-buffer
    "s-c" (if (featurep 'evil) 'evil-yank 'copy-region-as-kill)
    "s-v" #'yank
    ;; textmate-esque newline insertion
    [s-return]    #'evil-open-below
    [S-s-return]  #'evil-open-above
    ;; textmate-esque deletion
    [s-backspace] #'doom/backward-kill-to-bol-and-indent))


;;
;; Doom's keybinding scheme

(when (featurep! +bindings)
  ;; Ensure Emacsien motions are available
  (map! "C-b" #'backward-word
        "C-f" #'forward-word)

  (if (featurep 'evil)
      (load! "+evil-bindings")
    (load! "+emacs-bindings")))
