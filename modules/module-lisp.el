;;; module-lisp --- all things lisp
;; see lib/elisp-defuns.el

(add-hook! emacs-lisp-mode 'turn-on-eldoc-mode)

;; [pedantry intensifies]
(defadvice emacs-lisp-mode (after emacs-lisp-mode-rename-modeline activate)
  (setq mode-name "Elisp"))

(defun narf-elisp-auto-compile ()
  (when (narf/is-recompilable-p)
    (narf:compile-el)))

(add-hook! emacs-lisp-mode 'narf|enable-tab-width-8)
(add-hook! emacs-lisp-mode
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (add-hook 'after-save-hook 'narf-elisp-auto-compile nil t)

  (add-to-list 'imenu-generic-expression
               '("Package"
                 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))

  ;; Highlight extra NARF keywords
  (let ((keywords '("add-hook!"
                    "bind!"
                    "after!"
                    "λ"
                    "in!"
                    "define-company-backend!"
                    "define-text-object!"
                    "define-builder!"
                    )))
    (font-lock-add-keywords 'emacs-lisp-mode
                            `((,(concat "(\\s-*" (regexp-opt keywords 'paren) "\\_>")
                               1 font-lock-keyword-face)) 'append)))

(font-lock-add-keywords
 'emacs-lisp-mode `(("\\(lambda\\)" (0 (narf/show-as ?λ)))))

;; Real go-to-definition for elisp
(bind! :map emacs-lisp-mode-map
       :m "gd" 'narf/elisp-find-function-at-pt
       :m "gD" 'narf/elisp-find-function-at-pt-other-window)

(use-package slime :defer t
  :config
  (setq inferior-lisp-program "clisp"))

(provide 'module-lisp)
;;; module-elisp.el ends here
