;;; lang/coq/config.el -*- lexical-binding: t; -*-

(def-package! proof-site
  :load-path +coq-pg-loc
  :defer t
  :mode ("\\.v\\'" . coq-mode)
  :hook (coq-mode . company-coq-mode))
