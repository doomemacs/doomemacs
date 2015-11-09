;;; core-company.el --- auto completion backend (Company-mode)
;; see lib/company-macros.el

(eval-when-compile (require 'core))

(use-package company
  :diminish "="
  :commands (global-company-mode company-complete-common company-dict
             company-files company-tags company-ispell company-yasnippet
             company-semantic company-dabbrev-code)
  :init
  (after! abbrev (diminish 'abbrev-mode "A"))
  (setq company-idle-delay nil
        company-minimum-prefix-length 1
        company-show-numbers nil
        company-tooltip-limit 20
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-backends '((company-capf
                            company-yasnippet
                            company-dabbrev-code
                            company-keywords)
                           company-dabbrev)
        company-statistics-file (concat narf-temp-dir "company-stats-cache.el")
        company-dict-dir (concat narf-private-dir "dict/"))

  :config
  (require 'company-statistics)
  (require 'company-quickhelp)
  (setq company-quickhelp-delay nil
        ;; Rewrite evil-complete to use company-dabbrev
        company-dabbrev-code-other-buffers t
        evil-complete-next-func      'narf/company-evil-complete-next
        evil-complete-previous-func  'narf/company-evil-complete-previous)

  ;; TODO Restore company-dict
  ;; (use-package company-dict :defer t)

  (add-to-list 'company-transformers 'company-sort-by-occurrence)
  (setq-default company-backends (append '(company-keywords) company-backends))

  (define-company-backend! nxml-mode       (nxml yasnippet))
  (define-company-backend! emacs-lisp-mode (elisp yasnippet))

  (define-key company-active-map "\C-w" nil)

  (global-company-mode +1)
  (company-statistics-mode +1)
  (company-quickhelp-mode 1))

(provide 'core-company)
;;; core-company.el ends here
