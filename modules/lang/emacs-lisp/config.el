;;; lang/emacs-lisp/config.el

(add-hook! emacs-lisp-mode
  '(+emacs-lisp|init
    highlight-quoted-mode
    auto-compile-on-save-mode))

(defun +emacs-lisp|init ()
  (setq mode-name "Elisp") ; [pedantry intensifies]
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

  (font-lock-add-keywords
   nil `(("(\\(lambda\\)" (1 (ignore (compose-region (match-beginning 1) (match-end 1) ?λ 'decompose-region))))
         ("(\\(\\(doom\\)\\([-:/|!][^) ]*\\)?\\)[) \n]" (1 font-lock-builtin-face))
         ;; Highlight doom macros (macros are fontified in emacs 25+)
         ("(\\([^ ]+!\\)"
          (1 font-lock-keyword-face append))
         ;; Ert
         ("(\\(ert-deftest\\) \\([^ ]+\\)"
          (1 font-lock-keyword-face)
          (2 font-lock-function-name-face))))

  (dolist (i '(("Evil Command" "\\(^\\s-*(evil-define-command +\\)\\(\\_<[^ ]+\\_>\\)" 2)
               ("Evil Operator" "\\(^\\s-*(evil-define-operator +\\)\\(\\_<[^ ]+\\_>\\)" 2)
               ("Package" "\\(^\\s-*(\\(use-package\\|package!\\) +\\)\\(\\_<[^ \n]+\\_>\\)" 3)
               ("Spaceline Segment" "\\(^\\s-*(spaceline-define-segment +\\)\\(\\_<.+\\_>\\)" 2)))
    (push i imenu-generic-expression)))

(after! auto-compile
  (setq auto-compile-display-buffer nil
        auto-compile-use-mode-line nil))

(after! slime
  (setq inferior-lisp-program "clisp"))
