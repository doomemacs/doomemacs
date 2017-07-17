;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(def-package! elisp-mode ; built-in
  :mode ("/Cask$" . emacs-lisp-mode)
  :config
  (set! :repl 'emacs-lisp-mode #'+emacs-lisp/repl)
  (set! :eval 'emacs-lisp-mode #'+emacs-lisp-eval)
  (set! :jump 'emacs-lisp-mode :documentation #'describe-symbol)
  (set! :rotate 'emacs-lisp-mode
        :symbols '(("t" "nil")
                   ("let" "let*")
                   ("when" "unless")
                   ("append" "prepend")
                   ("advice-add" "advice-remove")
                   ("add-hook" "remove-hook")
                   ("add-hook!" "remove-hook!")))

  (add-hook! 'emacs-lisp-mode-hook
    #'(;; 3rd-party functionality
       eldoc-mode auto-compile-on-save-mode doom|enable-delete-trailing-whitespace
       ;; fontification
       rainbow-delimiters-mode highlight-quoted-mode highlight-numbers-mode +emacs-lisp|extra-fontification
       ;; initialization
       +emacs-lisp|init-imenu +emacs-lisp|init-flycheck))

  ;;
  (defun +emacs-lisp|extra-fontification ()
    "Display lambda as a smybol and fontify doom module functions."
    (font-lock-add-keywords
     nil `(;; Display "lambda" as λ
           ("(\\(lambda\\)" (1 (ignore (compose-region (match-beginning 1) (match-end 1) ?λ #'decompose-region))))
           ;; Highlight doom/module functions
           ("\\(^\\|\\s-\\|,\\)(\\(\\(doom\\|\\+\\)[^) ]+\\)[) \n]" (2 font-lock-keyword-face)))))

  (defun +emacs-lisp|init-imenu ()
    "Improve imenu support with better expression regexps and Doom-specific forms."
    (setq imenu-generic-expression
          '(("Evil Commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
            ("Package" "^\\s-*(\\(?:def-\\)?package! +\\(\\_<[^ ()\n]+\\_>\\)" 1)
            ("Settings" "^\\s-*(def-setting! +\\([^ ()\n]+\\)" 1)
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

  (defun +emacs-lisp|init-flycheck ()
    "Initialize flycheck-mode if not in emacs.d."
    (when (and buffer-file-name
               (not (file-in-directory-p buffer-file-name doom-emacs-dir)))
      (flycheck-mode +1))))


;;
;; Plugins
;;

(def-package! auto-compile
  :commands auto-compile-on-save-mode
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-use-mode-line nil))


(def-package! highlight-quoted
  :commands highlight-quoted-mode)


(def-package! slime
  :config
  (setq inferior-lisp-program "clisp")
  (require 'slime-fuzzy))


(def-package! macrostep
  :commands macrostep-expand
  :config
  (map! :map macrostep-keymap
        :n "RET"    #'macrostep-expand
        :n "e"      #'macrostep-expand
        :n "u"      #'macrostep-collapse
        :n "c"      #'macrostep-collapse

        :n "TAB"    #'macrostep-next-macro
        :n "n"      #'macrostep-next-macro
        :n "J"      #'macrostep-next-macro

        :n "S-TAB"  #'macrostep-prev-macro
        :n "K"      #'macrostep-prev-macro
        :n "p"      #'macrostep-prev-macro

        :n "q"      #'macrostep-collapse-all
        :n "C"      #'macrostep-collapse-all)
  ;; `evil-normalize-keymaps' seems to be required for macrostep or it won't
  ;; apply for the very first invocation
  (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps))


(def-package! flycheck-cask
  :when (featurep! :feature syntax-checker)
  :commands flycheck-cask-setup
  :init
  (add-hook! 'emacs-lisp-hook
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup nil t)))


(def-package! overseer
  :commands overseer-test
  :init (set! :popup "*overseer*" :size 12))


;;
;;
;;

(def-project-mode! +emacs-lisp-ert-mode
  :modes (emacs-lisp-mode)
  :match "/test[/-].+\\.el$")
