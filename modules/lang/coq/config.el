;;; lang/coq/config.el -*- lexical-binding: t; -*-

(def-package! proof-site
  :mode ("\\.v\\'" . coq-mode)
  :hook (coq-mode . company-coq-mode))
