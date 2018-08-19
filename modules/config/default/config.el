;;; config/default/config.el -*- lexical-binding: t; -*-

(if (featurep! +bindings) (load! "+bindings"))


;;
;; Plugins
;;

(def-package! emacs-snippets
  :if (featurep! +snippets)
  :after yasnippet)


;;
;; Config
;;

;; Don't store authinfo in non-encrypted files!
(defvar auth-sources
  (list (expand-file-name "authinfo.gpg" doom-etc-dir)
        "~/.authinfo.gpg"))

(after! epa
  (setq epa-file-encrypt-to (or epa-file-encrypt-to user-mail-address)
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

  ;; Major-mode specific fixes
  (sp-local-pair '(ruby-mode enh-ruby-mode) "{" "}"
                 :pre-handlers '(:rem sp-ruby-pre-handler)
                 :post-handlers '(:rem sp-ruby-post-handler))

  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; I likely don't want a new pair if adjacent to a word or opening brace
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

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
  (advice-add #'newline-and-indent :around #'doom*newline-and-indent))


(when (featurep 'evil)
  (when (featurep! +evil-commands)
    (load! "+evil-commands"))

  (when (featurep! +bindings)
    (defvar +default-repeat-forward-key ";")
    (defvar +default-repeat-backward-key ",")

    (eval-when-compile
      (defmacro do-repeat! (command next-func prev-func)
        "Makes ; and , the universal repeat-keys in evil-mode. These keys can be
customized by changing `+default-repeat-forward-key' and
`+default-repeat-backward-key'."
        (let ((fn-sym (intern (format "+evil*repeat-%s" (doom-unquote command)))))
          `(progn
             (defun ,fn-sym (&rest _)
               (define-key! evil-motion-state-map
                 +default-repeat-forward-key #',next-func
                 +default-repeat-backward-key #',prev-func))
             (advice-add #',command :before #',fn-sym)))))

    ;; n/N
    (do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
    (do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
    (do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
    (do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

    ;; f/F/t/T/s/S
    (setq evil-snipe-repeat-keys nil
          evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;
    (do-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
    (do-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse)

    ;; */#
    (do-repeat! evil-visualstar/begin-search-forward
                evil-ex-search-next evil-ex-search-previous)
    (do-repeat! evil-visualstar/begin-search-backward
                evil-ex-search-previous evil-ex-search-next)))
