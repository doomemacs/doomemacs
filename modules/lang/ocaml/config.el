;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

(def-package! tuareg
  :mode ("\\.ml[4ilpy]?\\'" . tuareg-mode))


(def-package! merlin
  :after tuareg
  :hook (tuareg-mode . merlin-mode)
  :config
  (set-company-backend! 'tuareg-mode 'merlin-company-backend)
  (after! company
    (remove-hook 'company-backends 'merlin-company-backend)))

(def-package! lsp-ocaml
  :when (featurep! +lsp)
  :when (featurep! :tools lsp)
  :hook ((tuareg-mode reason-mode) . lsp-ocaml-enable))
