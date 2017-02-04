;;; lang/emacs-lisp/config.el

(associate! emacs-lisp-mode :match "/Cask$")
(add-hook! emacs-lisp-mode '(highlight-quoted-mode auto-compile-on-save-mode +emacs-lisp|hook))

(after! elisp-mode
  ;; Real go-to-definition for elisp
  (map! :map emacs-lisp-mode-map
        :m "gd" '+emacs-lisp/find-function
        :leader :m "gd" '+emacs-lisp/find-function-other-window)

  ;; Don't affect lisp indentation (only `tab-width')
  (setq editorconfig-indentation-alist
        (delq (assq 'emacs-lisp-mode editorconfig-indentation-alist)
              editorconfig-indentation-alist))

  (defun +emacs-lisp|hook ()
    (setq mode-name "Elisp") ; [pedantry intensifies]
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

    (font-lock-add-keywords
     nil `(("(\\(lambda\\)" (1 (ignore (compose-region (match-beginning 1) (match-end 1) ?λ 'decompose-region))))
           ("[^'](\\(\\(doom\\)\\([-:/|!][^) ]*\\)?\\)[) \n]" (1 font-lock-builtin-face))
           ;; Highlight doom macros (macros are fontified in emacs 25+)
           ("[^'](\\([^ ]+!\\)"
            (1 font-lock-keyword-face append))
           ;; Ert
           ("[^'](\\(ert-deftest\\) \\([^ ]+\\)"
            (1 font-lock-keyword-face)
            (2 font-lock-function-name-face))))

    (dolist (i '(("Evil Command" "\\(^\\s-*(evil-define-command +\\)\\(\\_<[^ ]+\\_>\\)" 2)
                 ("Evil Operator" "\\(^\\s-*(evil-define-operator +\\)\\(\\_<[^ ]+\\_>\\)" 2)
                 ("Package" "\\(^\\s-*(\\(use-package\\|package\\)!? +\\)\\(\\_<[^ \n]+\\_>\\)" 3)
                 ("Spaceline Segment" "\\(^\\s-*(spaceline-define-segment +\\)\\(\\_<.+\\_>\\)" 2)))
      (push i imenu-generic-expression))))

(after! debug ;; elisp debugging
  (map! :map debugger-mode-map
        :n "RET" 'debug-help-follow
        :n "n"   'debugger-step-through
        :n "c"   'debugger-continue))


;;
;; Plugins
;;

(use-package! auto-compile
  :commands auto-compile-on-save-mode
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-use-mode-line nil))


(use-package! highlight-quoted
  :commands highlight-quoted-mode)


(use-package! slime
  :config (setq inferior-lisp-program "clisp"))

