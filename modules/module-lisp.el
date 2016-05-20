;;; module-lisp --- all things lisp

(associate! emacs-lisp-mode :match "\\(/Cask\\|\\.\\(el\\|gz\\)\\)$")
(add-hook! emacs-lisp-mode '(turn-on-eldoc-mode flycheck-mode highlight-numbers-mode))

(use-package highlight-quoted
  :commands (highlight-quoted-mode)
  :init (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(use-package slime :defer t
  :config (setq inferior-lisp-program "clisp"))

(add-hook 'emacs-lisp-mode-hook 'narf/elisp-init)
(defun narf/elisp-init ()
  (def-company-backend! emacs-lisp-mode (elisp yasnippet))
  (def-repl! emacs-lisp-mode narf/elisp-inf-ielm)
  (def-rotate! emacs-lisp-mode
    :symbols (("t" "nil")
              ("let" "let*")
              ("when" "unless")
              ("append" "prepend")
              ("advice-add" "advice-remove")
              ("add-hook" "add-hook!" "remove-hook")))

  ;; Don't affect lisp indentation (only `tab-width')
  (setq editorconfig-indentation-alist
        (delq (assq 'emacs-lisp-mode editorconfig-indentation-alist)
              editorconfig-indentation-alist))

  ;; Real go-to-definition for elisp
  (map! :map emacs-lisp-mode-map :m "gd" 'narf/elisp-find-function-at-pt)

  (remove-hook 'emacs-lisp-mode-hook 'narf/elisp-init))

(add-hook 'emacs-lisp-mode-hook 'narf/elisp-hook)
(defun narf/elisp-hook ()
  (setq mode-name "Elisp") ; [pedantry intensifies]

  (font-lock-add-keywords
   nil `(("(\\(lambda\\)"       (1 (narf/show-as ?λ)))
         ("(\\(narf\\)\\>" (1 font-lock-keyword-face append))
         ;; Highlight narf macros (macros are fontified in emacs 25+)
         (,(concat
            "(\\(def-"
            (regexp-opt '("electric" "project-type" "company-backend"
                          "builder" "repl" "textobj" "tmp-excmd" "rotate"
                          "repeat" "yas-mode" "version-cmd" "docset"
                          "open-with"))
            "!\\)")
          (1 font-lock-keyword-face append))
         (,(concat
            "(\\("
            (regexp-opt '("λ" "in" "map" "after" "shut-up" "add-hook"
                          "associate" "define-org-link" "ex"
                          "define-org-section"))
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
  (add-hook 'after-save-hook  'narf/elisp-auto-compile nil t)

  (dolist (i '(("Evil Command" "\\(^\\s-*(evil-define-command +\\)\\(\\_<.+\\_>\\)" 2)
               ("Evil Operator" "\\(^\\s-*(evil-define-operator +\\)\\(\\_<.+\\_>\\)" 2)
               ("Package" "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)
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
(def-project-type! emacs-ert "ert"
  :modes (emacs-lisp-mode)
  :match "/test/.+-test\\.el$"
  :bind (:localleader
          :n "tr" 'narf/ert-rerun-test
          :n "ta" 'narf/ert-run-all-tests
          :n "ts" 'narf/ert-run-test)
  (add-hook 'ert-results-mode-hook 'narf|hide-mode-line))

(provide 'module-lisp)
;;; module-elisp.el ends here
