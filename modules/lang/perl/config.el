;;; lang/perl/config.el -*- lexical-binding: t; -*-

;; There's also `perl-mode' for perl < 6, which is already set up.


(use-package! raku-mode
  :defer t
  :init
  (defalias 'perl6-mode #'raku-mode)
  :config
  (set-repl-handler! 'raku-mode #'run-raku))


(use-package! flycheck-raku
  :when (featurep! :checkers syntax)
  :after raku-mode)
