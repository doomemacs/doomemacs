;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(defvar +emacs-lisp-enable-extra-fontification t
  "If non-nil, fontify built-in functions and variables especially (symbols
defined by Emacs, not Doom or packages). This can help make typos stand out.")


;;
;; elisp-mode deferral hack
;;

;; `elisp-mode' is loaded at startup. In order to lazy load its config we need
;; to pretend it isn't loaded
(delq 'elisp-mode features)
;; ...until the first time `emacs-lisp-mode' runs
(advice-add #'emacs-lisp-mode :before #'+emacs-lisp|init)

(defun +emacs-lisp|init (&rest _)
  ;; Some plugins (like yasnippet) run `emacs-lisp-mode' early, to parse some
  ;; elisp. This would prematurely trigger this function. In these cases,
  ;; `emacs-lisp-mode-hook' is let-bound to nil or its hooks are delayed, so if
  ;; we see either, keep pretending elisp-mode isn't loaded.
  (when (and emacs-lisp-mode-hook (not delay-mode-hooks))
    ;; Otherwise, announce to the world elisp-mode has been loaded, so `after!'
    ;; handlers can respond and configure elisp-mode as expected.
    (provide 'elisp-mode)
    (advice-remove #'emacs-lisp-mode #'+emacs-lisp|init)))


;;
;; Config
;;

(add-to-list 'auto-mode-alist '("\\.Cask\\'" . emacs-lisp-mode))

(after! elisp-mode
  (set-repl-handler! 'emacs-lisp-mode #'+emacs-lisp/repl)
  (set-eval-handler! 'emacs-lisp-mode #'+emacs-lisp-eval)
  (set-lookup-handlers! 'emacs-lisp-mode :documentation 'info-lookup-symbol)
  (set-docset! 'emacs-lisp-mode "Emacs Lisp")
  (set-pretty-symbols! 'emacs-lisp-mode :lambda "lambda")
  (set-rotate-patterns! 'emacs-lisp-mode
    :symbols '(("t" "nil")
               ("let" "let*")
               ("when" "unless")
               ("append" "prepend")
               ("advice-add" "advice-remove")
               ("add-hook" "remove-hook")
               ("add-hook!" "remove-hook!")))

  ;; variable-width indentation is superior in elisp
  (add-to-list 'doom-detect-indentation-excluded-modes 'emacs-lisp-mode nil #'eq)

  (add-hook! 'emacs-lisp-mode-hook
    #'(;; 3rd-party functionality
       auto-compile-on-save-mode doom|enable-delete-trailing-whitespace
       ;; fontification
       rainbow-delimiters-mode highlight-quoted-mode
       ;; initialization
       +emacs-lisp|init-imenu +emacs-lisp|disable-flycheck-maybe))

  ;; Special fontification for doom
  (font-lock-add-keywords
   'emacs-lisp-mode
   `(;; custom Doom cookies
     ("^;;;###\\(autodef\\|if\\)[ \n]" (1 font-lock-warning-face t))
     ;; doom/module functions
     ("\\(^\\|\\s-\\|,\\)(\\(\\(doom\\|\\+\\)[^) ]+\\|[^) ]+!\\)[) \n]" (2 'font-lock-keyword-face))))

  ;; Highlight symbols in standard library
  (when +emacs-lisp-enable-extra-fontification
    (load! "+symbols")
    (font-lock-add-keywords
     'emacs-lisp-mode
     `((,(concat "\\(?:(\\|#'\\)" (regexp-opt +emacs-lisp-function-list t) "\\_>") (1 'font-lock-function-name-face))
       (,(concat "\\(?:(\\|#'\\)" (regexp-opt +emacs-lisp-command-list t) "\\_>")  (1 'font-lock-function-name-face))
       (,(regexp-opt +emacs-lisp-variable-list 'symbols) . font-lock-variable-name-face)
       (,(regexp-opt +emacs-lisp-option-list 'symbols)   . font-lock-variable-name-face))))

  (defun +emacs-lisp|init-imenu ()
    "Improve imenu support with better expression regexps and Doom-specific forms."
    (setq imenu-generic-expression
          '(("Evil Commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
            ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
            ("Package" "^\\s-*(\\(?:def-\\)?package! +\\(\\_<[^ ()\n]+\\_>\\)" 1)
            ("Settings" "^\\s-*(def-setting! +\\([^ ()\n]+\\)" 1)
            ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
            ("Modelines" "^\\s-*(def-modeline! +\\([^ ()\n]+\\)" 1)
            ("Modeline Segments" "^\\s-*(def-modeline-segment! +\\([^ ()\n]+\\)" 1)
            ("Advice" "^\\s-*(def\\(?:\\(?:ine-\\)?advice\\))")
            ("Modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
            ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
            ("Inline Functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
            ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]+\\)" 1)
            ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
            ("Variables" "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]" 1)
            ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))))

  (defun +emacs-lisp|disable-flycheck-maybe ()
    "Disable flycheck-mode if in emacs.d."
    (when (or (not buffer-file-name)
              (cl-loop for dir in (list doom-emacs-dir doom-private-dir)
                       if (file-in-directory-p buffer-file-name dir)
                       return t))
      (flycheck-mode -1))))


;;
;; Plugins
;;

;; `auto-compile'
(setq auto-compile-display-buffer nil
      auto-compile-use-mode-line nil)


;; `macrostep'
(when (featurep! :feature evil)
  (after! macrostep
    (evil-define-key* 'normal macrostep-keymap
      (kbd "RET") #'macrostep-expand
      "e"         #'macrostep-expand
      "u"         #'macrostep-collapse
      "c"         #'macrostep-collapse

      [tab]       #'macrostep-next-macro
      "\C-n"      #'macrostep-next-macro
      "J"         #'macrostep-next-macro

      [backtab]   #'macrostep-prev-macro
      "K"         #'macrostep-prev-macro
      "\C-p"      #'macrostep-prev-macro

      "q"         #'macrostep-collapse-all
      "C"         #'macrostep-collapse-all)

    ;; `evil-normalize-keymaps' seems to be required for macrostep or it won't
    ;; apply for the very first invocation
    (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps)))


(def-package! flycheck-cask
  :when (featurep! :feature syntax-checker)
  :defer t
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t)))


;;
;; Project modes
;;

(def-project-mode! +emacs-lisp-ert-mode
  :modes (emacs-lisp-mode)
  :match "/test[/-].+\\.el$")

(associate! buttercup-minor-mode
  :modes (emacs-lisp-mode)
  :match "/test[/-].+\\.el$")

(after! buttercup
  (set-yas-minor-mode! 'buttercup-minor-mode))

