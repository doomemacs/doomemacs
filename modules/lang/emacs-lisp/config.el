;;; lang/emacs-lisp/config.el

(def-package! elisp-mode ; built-in
  :mode ("/Cask$" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook '+emacs-lisp|hook)

  :config
  (map! :map emacs-lisp-mode-map
        :m "gd" '+emacs-lisp/find-function
        :leader :m "gd" '+emacs-lisp/find-function-in-other-window)

  (set! :rotate 'emacs-lisp-mode
        :symbols '(("t" "nil")
                   ("let" "let*")
                   ("when" "unless")
                   ("append" "prepend")
                   ("advice-add" "advice-remove")
                   ("add-hook" "add-hook!" "remove-hook")))

  ;; Don't affect lisp indentation (only `tab-width')
  (setq editorconfig-indentation-alist
        (delq (assq 'emacs-lisp-mode editorconfig-indentation-alist)
              editorconfig-indentation-alist))

  (defun +emacs-lisp|hook ()
    (setq mode-name "Elisp") ; [pedantry intensifies]
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

    (eldoc-mode +1)
    (highlight-quoted-mode +1)
    (auto-compile-on-save-mode +1)

    (font-lock-add-keywords
     nil `(;; Display "lambda" as λ
           ("(\\(lambda\\)" (1 (ignore (compose-region (match-beginning 1) (match-end 1) ?λ 'decompose-region))))
           ;; Highlight doom/module functions
           ("\\(^\\|\\s-\\|,\\)(\\(\\(doom\\|\\+\\)[^) ]+\\)[) \n]" (2 font-lock-builtin-face))))

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
            ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\) +\\([^ )\n]+\\)" 1)
            ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
            ("Variables" "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]" 1)
            ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
            ))))


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
  :config (setq inferior-lisp-program "clisp"))

