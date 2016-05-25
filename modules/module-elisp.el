;;; module-elisp --- all things lisp

(associate! emacs-lisp-mode :match "\\(/Cask\\|\\.\\(el\\|gz\\)\\)$")
(add-hook! emacs-lisp-mode '(turn-on-eldoc-mode flycheck-mode highlight-numbers-mode))

;; Real go-to-definition for elisp
(map! :map emacs-lisp-mode-map :m "gd" 'doom/elisp-find-function-at-pt)

(add-hook 'emacs-lisp-mode-hook 'doom/elisp-init)
(defun doom/elisp-init ()
  (def-company-backend! emacs-lisp-mode (elisp yasnippet))
  (def-docset! emacs-lisp-mode ("Emacs Lisp"))
  (def-repl! emacs-lisp-mode doom/elisp-inf-ielm)
  (def-rotate! emacs-lisp-mode
    :symbols (("t" "nil")
              ("let" "let*")
              ("when" "unless")
              ("append" "prepend")
              ("advice-add" "advice-remove")
              ("add-hook" "add-hook!" "remove-hook")))

  (def-popup! "*ert*" :align below :size 20 :noselect t)

  ;; Don't affect lisp indentation (only `tab-width')
  (setq editorconfig-indentation-alist
        (delq (assq 'emacs-lisp-mode editorconfig-indentation-alist)
              editorconfig-indentation-alist))

  (remove-hook 'emacs-lisp-mode-hook 'doom/elisp-init))

(add-hook 'emacs-lisp-mode-hook 'doom/elisp-hook)
(defun doom/elisp-hook ()
  (setq mode-name "Elisp") ; [pedantry intensifies]

  (font-lock-add-keywords
   nil `(("(\\(lambda\\)" (1 (doom/show-as ?λ)))
         ("(\\(\\(doom\\)\\([-:/|][^) ]*\\)?\\)[) ]" (1 font-lock-builtin-face))
         ;; Highlight doom macros (macros are fontified in emacs 25+)
         (,(concat
            "(\\(def-"
            (regexp-opt '("electric" "project-type" "company-backend"
                          "builder" "repl" "text-obj" "tmp-excmd" "rotate"
                          "repeat" "yas-mode" "version-cmd" "docset" "popup"
                          "open-with"))
            "!\\)")
          (1 font-lock-keyword-face append))
         (,(concat
            "(\\("
            (regexp-opt '("λ" "in" "map" "after" "shut-up" "add-hook"
                          "associate" "define-org-link" "ex"
                          "define-org-section" "set" "noop"))
            "!\\)")
          (1 font-lock-keyword-face append))
         ;; Ert

         (,(concat
            "("
            (regexp-opt '("ert-deftest") t)
            " \\([^ ]+\\)")
          (1 font-lock-keyword-face)
          (2 font-lock-function-name-face))))

  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

  (dolist (i '(("Evil Command" "\\(^\\s-*(evil-define-command +\\)\\(\\_<.+\\_>\\)" 2)
               ("Evil Operator" "\\(^\\s-*(evil-define-operator +\\)\\(\\_<.+\\_>\\)" 2)
               ("Package" "\\(^\\s-*(use-package +\\)\\(\\_<[^ \n]+\\_>\\)" 2)
               ("Spaceline Segment" "\\(^\\s-*(spaceline-define-segment +\\)\\(\\_<.+\\_>\\)" 2)))
    (push i imenu-generic-expression)))

;; Add new colors to helm-imenu
(after! helm-imenu
  (defun helm-imenu-transformer (candidates)
    (cl-loop for (k . v) in candidates
             for types = (or (helm-imenu--get-prop k)
                             (list "Function" k))
             for bufname = (buffer-name (marker-buffer v))
             for disp1 = (mapconcat
                          (lambda (x)
                            (propertize
                             x 'face (cond ((string= x "Variables")
                                            'font-lock-variable-name-face)
                                           ((or (string= x "Function")
                                                (string-prefix-p "Evil " x t))
                                            'font-lock-function-name-face)
                                           ((string= x "Types")
                                            'font-lock-type-face)
                                           ((string= x "Package")
                                            'font-lock-negation-char-face)
                                           ((string= x "Spaceline Segment")
                                            'font-lock-string-face))))
                          types helm-imenu-delimiter)
             for disp = (propertize disp1 'help-echo bufname)
             collect
             (cons disp (cons k v)))))


;;
(use-package highlight-quoted
  :commands (highlight-quoted-mode)
  :init (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(use-package slime :defer t
  :config (setq inferior-lisp-program "clisp"))

(use-package auto-compile
  :commands (auto-compile-on-save-mode)
  :init (add-hook 'emacs-lisp-mode-hook 'auto-compile-on-save-mode)
  :config (setq auto-compile-display-buffer nil))


;;
(def-project-type! emacs-ert "ert"
  :modes (emacs-lisp-mode)
  :match "/test/.+-test\\.el$"
  :bind (:localleader
          :n "tr" 'doom/ert-rerun-test
          :n "ta" 'doom/ert-run-all-tests
          :n "ts" 'doom/ert-run-test)
  (add-hook 'ert-results-mode-hook 'doom|hide-mode-line))

(provide 'module-elisp)
;;; module-elisp.el ends here
