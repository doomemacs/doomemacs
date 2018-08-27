;;; lang/coq/config.el -*- lexical-binding: t; -*-

(after! company-coq
  (when (not (featurep! :completion company))
    (setq company-coq-disabled-features '(company company-defaults))))
