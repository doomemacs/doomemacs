;;; lang/perl/config.el -*- lexical-binding: t; -*-

;; There's also `perl-mode' for perl < 6, which is already set up.

;; REVIEW Until Raku/raku-mode#33 is merged.
(use-package! raku-mode
  :interpreter "raku"
  :mode "\\.nqp\\'"
  :mode "\\.raku\\(mod\\|test\\)?"
  :init (defalias 'perl6-mode #'raku-mode)
  :config
  (set-repl-handler! 'raku-mode #'run-raku))


(use-package! flycheck-raku
  :when (featurep! :checkers syntax)
  :after raku-mode)
