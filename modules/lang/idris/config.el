;;; lang/idris/config.el -*- lexical-binding: t; -*-

(after! idris-mode
  (add-hook! 'idris-mode-hook 'turn-on-idris-simple-indent)
  (set-repl-handler! 'idris-mode 'idris-pop-to-repl)
  (set-lookup-handlers! 'idris-mode
    :documentation #'idris-docs-at-point
    :file #'idris-load-file)
  (map! :map idris-mode-map
        :localleader
        :n "r" #'idris-load-file
        :n "t" #'idris-type-at-point
        :n "d" #'idris-add-clause
        :n "l" #'idris-make-lemma
        :n "c" #'idris-case-split
        :n "w" #'idris-make-with-block
        :n "m" #'idris-add-missing
        :n "p" #'idris-proof-search
        :n "h" #'idris-docs-at-point))
