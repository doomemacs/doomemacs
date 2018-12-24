;;; config/default/config.el -*- lexical-binding: t; -*-

;; Don't store authinfo in plain text!
(setq auth-sources
      (list (expand-file-name "authinfo.gpg" doom-etc-dir)
            "~/.authinfo.gpg"))

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


;; disable :unless predicates with (sp-pair "'" nil :unless nil)
;; disable :post-handlers with (sp-pair "{" nil :post-handlers nil)
;; ...or specific :post-handlers with (sp-pair "{" nil :post-handlers '(:rem ("| " "SPC")))
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
   '(js2-mode typescript-mode rjsx-mode rust-mode
     c-mode c++-mode objc-mode java-mode php-mode
     css-mode scss-mode less-css-mode stylus-mode)
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
  ;;  e) properly delete smartparen pairs when they are encountered, without the
  ;;     need for strict mode.
  ;;  f) do none of this when inside a string
  (advice-add #'delete-backward-char :override #'doom/delete-backward-char)

  ;; Makes `newline-and-indent' smarter when dealing with comments
  (advice-add #'newline-and-indent :around #'doom*newline-indent-and-continue-comments))


;;
;; Doom's keybinding scheme

(when (featurep! +bindings)
  (if (featurep 'evil)
      (load! "+evil-bindings")
    (load! "+emacs-bindings")))
