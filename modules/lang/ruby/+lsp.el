;;; lang/ruby/+lsp.el -*- lexical-binding: t; -*-

(lsp-define-stdio-client lsp-solargraph
                         "ruby"
                         #'projectile-project-root
                         `(,(executable-find "solargraph") "stdio")
                         :ignore-regexps
                         '("^Solargraph is listening on stdio PID=*$"))

(add-hook! 'enh-ruby-mode #'lsp-solargraph-enable)
(after! 'enh-ruby-mode
  (set-company-backend! 'enh-ruby-mode 'company-lsp))
;;; +lsp.el ends here
