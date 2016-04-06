;;; core-completion.el --- auto completion backend (Company-mode)

(use-package company
  :init
  (setq company-idle-delay nil
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-backends  '((company-capf company-dabbrev-code)
                            company-dabbrev))

  :config
  ;; Rewrites evil-complete to use company-dabbrev
  (setq evil-complete-next-func      'narf/company-evil-complete-next
        evil-complete-previous-func  'narf/company-evil-complete-previous)

  (push 'company-sort-by-occurrence company-transformers)
  (setq-default company-backends (append '(company-keywords) company-backends))

  (define-company-backend! nxml-mode       (nxml yasnippet))
  (define-company-backend! emacs-lisp-mode (elisp yasnippet))

  (define-key company-active-map "\C-w" nil)

  (global-company-mode +1)

  ;; NOTE: pos-tip.el in Emacs 25+ does not work
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode +1)
    (setq company-quickhelp-delay nil))

  (use-package company-statistics
    :config
    (company-statistics-mode +1)
    (setq company-statistics-file (concat narf-temp-dir "/company-stats-cache.el"))))

(use-package company-dict :defer t
  :config (setq company-dict-dir (concat narf-private-dir "/dict")))

(provide 'core-completion)
;;; core-completion.el ends here
