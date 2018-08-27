;;; lang/coq/config.el -*- lexical-binding: t; -*-

(after! coq
  (setq proof-electric-terminator-enable t))

(after! company-coq
  (set-lookup-handlers! 'company-coq-mode
    :definition #'company-coq-jump-to-definition
    :references #'company-coq-grep-symbol
    :documentation #'company-coq-doc)
  (when (not (featurep! :completion company))
    (setq company-coq-disabled-features '(company company-defaults))))
