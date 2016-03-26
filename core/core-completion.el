;;; core-completion.el --- auto completion backend (Company-mode)

(use-package company
  :commands (global-company-mode company-complete company-complete-common company-dict
             company-files company-tags company-ispell company-yasnippet
             company-semantic company-dabbrev-code company-manual-begin)
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
        company-backends '((company-capf
                            company-yasnippet
                            company-dabbrev-code)
                           company-dabbrev)
        company-statistics-file (concat narf-temp-dir "/company-stats-cache.el")
        company-quickhelp-delay nil)

  :config
  (use-package company-statistics)

  ;; NOTE: pos-tip.el in Emacs 25+ does not work
  (use-package company-quickhelp :config (company-quickhelp-mode +1))

  (use-package company-dict :defer t
    :config
    (setq company-dict-dir (concat narf-private-dir "dict")))

  ;; Rewrites evil-complete to use company-dabbrev
  (setq evil-complete-next-func      'narf/company-evil-complete-next
        evil-complete-previous-func  'narf/company-evil-complete-previous)

  (add-to-list 'company-transformers 'company-sort-by-occurrence)
  (setq-default company-backends (append '(company-keywords) company-backends))

  (define-company-backend! nxml-mode       (nxml yasnippet))
  (define-company-backend! emacs-lisp-mode (elisp yasnippet))

  (define-key company-active-map "\C-w" nil)

  (global-company-mode +1)
  (company-statistics-mode +1))

(provide 'core-completion)
;;; core-completion.el ends here
