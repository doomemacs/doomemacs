;;; core-company.el --- auto completion backend (Company-mode)
;; see lib/company-macros.el

(eval-when-compile (require 'core))

(use-package company
  :diminish (company-mode . "=")
  :commands (company-complete-common company-files company-tags
             company-ispell company-yasnippet company-semantic
             company-dabbrev-code)
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
        company-dict-dir (concat narf-private-dir "dict/"))
  :config
  (global-company-mode +1)

  ;; (use-package company-dict :defer t)
  ;; (setq-default company-backends (append '(company-dict company-keywords) company-backends))

  (setq-default company-backends (append '(company-keywords) company-backends))
  ;; TODO: Investigate yasnippet
  (after! yasnippet
    (setq-default company-backends (append '(company-capf company-yasnippet) company-backends)))
  (add-to-list 'company-transformers 'company-sort-by-occurrence)

  (add-company-backend! nxml-mode       (nxml yasnippet))
  (add-company-backend! emacs-lisp-mode (elisp yasnippet))

  ;; Rewrite evil-complete to use company-dabbrev
  (setq company-dabbrev-code-other-buffers t
        company-dabbrev-code-buffers       nil
        evil-complete-next-func           'narf/company-evil-complete-next
        evil-complete-previous-func       'narf/company-evil-complete-previous)

  (shut-up!
    (setq company-statistics-file (! (concat narf-temp-dir "company-statistics-cache.el")))
    (require 'company-statistics)
    (company-statistics-mode)))

(provide 'core-company)
;;; core-company.el ends here
