;;; lang/data/config.el -*- lexical-binding: t; -*-

(use-package! nxml-mode
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"   ; xslt, xsd
  :mode "\\.rss\\'"
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t)
  ;; https://github.com/Fuco1/smartparens/issues/397#issuecomment-501059014
  (after! smartparens
    (sp-local-pair 'nxml-mode "<" ">" :post-handlers '(("[d1]" "/"))))
  (set-company-backend! 'nxml-mode '(company-nxml company-yasnippet))
  (setq-hook! 'nxml-mode-hook tab-width nxml-child-indent)
  (set-formatter! 'xmllint '("xmllint" "--format" "-") :modes '(nxml-mode)))


;;;###package csv-mode
(map! :after csv-mode
      :localleader
      :map csv-mode-map
      "a" #'csv-align-fields
      "u" #'csv-unalign-fields
      "s" #'csv-sort-fields
      "S" #'csv-sort-numeric-fields
      "k" #'csv-kill-fields
      "t" #'csv-transpose)
