;;; module-lisp --- all things lisp

(associate! emacs-lisp-mode :match "\\(/Cask\\|\\.\\(el\\|gz\\)\\)$")
(def-company-backend! emacs-lisp-mode (elisp yasnippet))
(def-repl! emacs-lisp-mode narf/elisp-inf-ielm)

(add-hook! emacs-lisp-mode
  '(turn-on-eldoc-mode flycheck-mode highlight-numbers-mode))

;; Real go-to-definition for elisp
(map! :map emacs-lisp-mode-map :m "gd" 'narf/elisp-find-function-at-pt)

(use-package highlight-quoted
  :commands (highlight-quoted-mode)
  :init (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(use-package slime :defer t
  :config (setq inferior-lisp-program "clisp"))

;; Don't affect lisp indentation (only `tab-width')
(setq editorconfig-indentation-alist
      (delq (assq 'emacs-lisp-mode editorconfig-indentation-alist)
            editorconfig-indentation-alist))

(add-hook! emacs-lisp-mode
  (setq mode-name "Elisp") ; [pedantry intensifies]

  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (add-hook 'after-save-hook  'narf/elisp-auto-compile nil t)

  (let ((header-face 'font-lock-constant-face))
    (add-to-list 'imenu-generic-expression
                 `("Evil Command" "\\(^\\s-*(evil-define-command +\\)\\(\\_<.+\\_>\\)" 2))
    (add-to-list 'imenu-generic-expression
                 `("Evil Operator" "\\(^\\s-*(evil-define-operator +\\)\\(\\_<.+\\_>\\)" 2))
    (add-to-list 'imenu-generic-expression
                 `("Package" "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))
    (add-to-list 'imenu-generic-expression
                 `("Spaceline Segment" "\\(^\\s-*(spaceline-define-segment +\\)\\(\\_<.+\\_>\\)" 2))))

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

(font-lock-add-keywords
 'emacs-lisp-mode `(("(\\(lambda\\)"
                     (1 (narf/show-as ?λ)))
                    ;; Highlight narf macros (macros are fontified in emacs 25+)
                    (,(concat
                       "(\\(def-"
                       (regexp-opt '("electric" "project-type" "company-backend"
                                     "builder" "repl" "textobj" "tmp-excmd"
                                     "repeat" "yas-mode" "env-command" "docset"))
                       "!\\)")
                     (1 font-lock-keyword-face append))
                    (,(concat
                       "(\\("
                       (regexp-opt '("λ" "in" "map" "after" "shut-up" "add-hook"
                                     "associate" "open-with" "define-org-link"
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
