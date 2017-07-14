;;; lang/perl/config.el -*- lexical-binding: t; -*-

;; There's also `perl-mode' for perl < 6, which is already set up.
(when (featurep! :feature syntax-checker)
  (add-hook 'perl-mode-hook #'flycheck-mode))


(def-package! perl6-mode
  :init (require 'perl6-detect))


(def-package! flycheck-perl6
  :after perl6-mode
  :when (featurep! :feature syntax-checker)
  :config (add-hook 'perl6-mode-hook #'flycheck-mode))
