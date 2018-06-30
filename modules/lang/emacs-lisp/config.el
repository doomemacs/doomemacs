;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

;; `elisp-mode' is always loaded at startup, so to lazy load its config we need
;; to be creative. So we configure it the first tiem `emacs-lisp-mode' is run.
(advice-add #'emacs-lisp-mode :before #'+emacs-lisp|init)
;; And we remove `elisp-mode' so later invokations of (after! elisp-mode ...)
;; work as expected.
(delq 'elisp-mode features)

(defun +emacs-lisp|init (&rest _)
  ;; Some plugins (like yasnippet) will run `emacs-lisp-mode' early, prematurely
  ;; triggering this function in a non-ideal environment (`emacs-lisp-mode-hook'
  ;; is let-bound to nil). This breaks a lot of Doom setters, because they try
  ;; to add hooks to `emacs-lisp-mode-hook'!
  ;;
  ;; This means, in some sessions, elisp-mode is never configured properly, so
  ;; we have to make extra sure `emacs-lisp-mode' was executed interactively.
  (when (and emacs-lisp-mode-hook (not delay-mode-hooks))
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
  (set-docset! '(lisp-mode emacs-lisp-mode) "Emacs Lisp")
  (set-pretty-symbols! 'emacs-lisp-mode :lambda "lambda")
  (set-rotate-patterns! 'emacs-lisp-mode
    :symbols '(("t" "nil")
               ("let" "let*")
               ("when" "unless")
               ("append" "prepend")
               ("advice-add" "advice-remove")
               ("add-hook" "remove-hook")
               ("add-hook!" "remove-hook!")))

  (add-hook! 'emacs-lisp-mode-hook
    #'(;; 3rd-party functionality
       auto-compile-on-save-mode doom|enable-delete-trailing-whitespace
       ;; fontification
       rainbow-delimiters-mode highlight-quoted-mode highlight-numbers-mode +emacs-lisp|extra-fontification
       ;; initialization
       +emacs-lisp|init-imenu +emacs-lisp|disable-flycheck-maybe))

  (defun +emacs-lisp|extra-fontification ()
    "Display lambda as a smybol and fontify doom module functions."
    (font-lock-add-keywords
     nil `(;; Highlight custom Doom cookies
           ("^;;;###\\(autodef\\|if\\)[ \n]" (1 font-lock-warning-face t))
           ;; Highlight doom/module functions
           ("\\(^\\|\\s-\\|,\\)(\\(\\(doom\\|\\+\\)[^) ]+\\|[^) ]+!\\)[) \n]" (2 font-lock-keyword-face)))))

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

