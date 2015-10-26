;;; core-company.el --- auto completion backend (Company-mode)
;; see lib/company-macros.el

(eval-when-compile (require 'core))

(use-package company
  :diminish (company-mode . "=")
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
        company-global-modes '(not eshell-mode comint-mode org-mode erc-mode
                                   message-mode help-mode)
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-echo-metadata-frontend
                            company-preview-if-just-one-frontend)
        company-backends '((company-capf
                            company-yasnippet
                            company-dabbrev-code
                            company-files
                            company-keywords)
                           company-dabbrev)
        company-dict-dir (concat narf-private-dir "dict/"))
  :config
  (add-to-list 'company-transformers 'company-sort-by-occurrence)
  (setq-default company-backends (append '(company-keywords) company-backends))
  (after! yasnippet
    (setq-default company-backends (append '(company-capf company-yasnippet) company-backends)))

  (define-company-backend! nxml-mode       (nxml yasnippet))
  (define-company-backend! emacs-lisp-mode (elisp yasnippet))

  ;; (use-package company-dict :defer t)
  ;; (setq-default company-backends (append '(company-dict company-keywords) company-backends))

  ;; Rewrite evil-complete to use company-dabbrev
  (setq company-dabbrev-code-other-buffers t
        evil-complete-next-func           'narf/company-evil-complete-next
        evil-complete-previous-func       'narf/company-evil-complete-previous)

  (define-key company-active-map (kbd "C-w") nil)

  (shut-up!
    (setq company-statistics-file (concat narf-temp-dir "company-stats-cache.el"))
    (require 'company-statistics)
    (company-statistics-mode))

  (global-company-mode +1))

(provide 'core-company)
;;; core-company.el ends here
