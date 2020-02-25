;;; lang/perl/config.el -*- lexical-binding: t; -*-

;; There's also `perl-mode' for perl < 6, which is already set up.

(use-package! perl6-detect)


(use-package! flycheck-perl6
  :when (featurep! :checkers syntax)
  :after perl6-mode)
