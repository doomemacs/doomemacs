;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, highlight special forms, and defined functions and variables.")

;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(defer-feature! elisp-mode emacs-lisp-mode)


;;
;; Config

(add-to-list 'auto-mode-alist '("\\.Cask\\'" . emacs-lisp-mode))

(after! elisp-mode
  (set-repl-handler! 'emacs-lisp-mode #'+emacs-lisp/repl)
  (set-eval-handler! 'emacs-lisp-mode #'+emacs-lisp-eval)
  (set-lookup-handlers! 'emacs-lisp-mode
    :definition    #'elisp-def
    :documentation #'info-lookup-symbol)
  (set-docsets! 'emacs-lisp-mode "Emacs Lisp")
  (set-pretty-symbols! 'emacs-lisp-mode :lambda "lambda")
  (set-rotate-patterns! 'emacs-lisp-mode
    :symbols '(("t" "nil")
               ("let" "let*")
               ("when" "unless")
               ("append" "prepend")
               ("advice-add" "advice-remove")
               ("add-hook" "remove-hook")
               ("add-hook!" "remove-hook!")))

  (setq-hook! 'emacs-lisp-mode-hook
    ;; shorter name in modeline
    mode-name "Elisp"
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp ";;;;* [^ \t\n]")

  ;; variable-width indentation is superior in elisp
  (add-to-list 'doom-detect-indentation-excluded-modes 'emacs-lisp-mode nil #'eq)

  (add-hook! 'emacs-lisp-mode-hook
    #'(;; 3rd-party functionality
       auto-compile-on-save-mode outline-minor-mode
       ;; initialization
       +emacs-lisp|extend-imenu))

  ;; Flycheck produces a *lot* of false positives in emacs configs, so disable
  ;; it when you're editing them
  (add-hook 'flycheck-mode-hook #'+emacs-lisp|disable-flycheck-maybe)

  ;; Special fontification for elisp
  (font-lock-add-keywords
   'emacs-lisp-mode
   (append `(;; custom Doom cookies
             ("^;;;###\\(autodef\\|if\\)[ \n]" (1 font-lock-warning-face t)))
           ;; highlight defined, special variables & functions
           (when +emacs-lisp-enable-extra-fontification
             `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))

  (add-hook! 'emacs-lisp-mode-hook #'(rainbow-delimiters-mode highlight-quoted-mode))

  ;; Recenter window after following definition
  (advice-add #'elisp-def :after #'doom*recenter)

  (map! :localleader
        :map emacs-lisp-mode-map
        "e" #'macrostep-expand))


;;
;; Packages

;; `auto-compile'
(setq auto-compile-display-buffer nil
      auto-compile-use-mode-line nil)


;; `macrostep'
(when (featurep! :feature evil)
  (after! macrostep
    (evil-define-key* 'normal macrostep-keymap
      [return]  #'macrostep-expand
      "e"       #'macrostep-expand
      "u"       #'macrostep-collapse
      "c"       #'macrostep-collapse

      [tab]     #'macrostep-next-macro
      "\C-n"    #'macrostep-next-macro
      "J"       #'macrostep-next-macro

      [backtab] #'macrostep-prev-macro
      "K"       #'macrostep-prev-macro
      "\C-p"    #'macrostep-prev-macro

      "q"       #'macrostep-collapse-all
      "C"       #'macrostep-collapse-all)

    ;; `evil-normalize-keymaps' seems to be required for macrostep or it won't
    ;; apply for the very first invocation
    (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps)))


;; `overseer'
(autoload 'overseer-test "overseer" nil t)


(def-package! flycheck-cask
  :when (featurep! :feature syntax-checker)
  :defer t
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t)))


;;
;; Project modes

(def-project-mode! +emacs-lisp-ert-mode
  :modes (emacs-lisp-mode)
  :match "/test[/-].+\\.el$")

(associate! buttercup-minor-mode
  :modes (emacs-lisp-mode)
  :match "/test[/-].+\\.el$")

(after! buttercup
  (set-yas-minor-mode! 'buttercup-minor-mode))

