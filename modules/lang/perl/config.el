;;; lang/perl/config.el -*- lexical-binding: t; -*-

;; There's also `perl-mode' for perl < 6, which is already set up.

(def-package! perl6-detect)


(def-package! flycheck-perl6
  :when (featurep! :feature syntax-checker)
  :after perl6-mode)
