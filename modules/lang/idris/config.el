;;; lang/idris/config.el -*- lexical-binding: t; -*-

(after! idris-mode
  (add-hook 'idris-mode-hook #'turn-on-idris-simple-indent)
  (set-repl-handler! 'idris-mode 'idris-pop-to-repl)
  (set-lookup-handlers! 'idris-mode
    :documentation #'idris-docs-at-point
    :file #'idris-load-file)
  (map! :localleader
        :map idris-mode-map
        "r" #'idris-load-file
        "t" #'idris-type-at-point
        "d" #'idris-add-clause
        "l" #'idris-make-lemma
        "c" #'idris-case-split
        "w" #'idris-make-with-block
        "m" #'idris-add-missing
        "p" #'idris-proof-search
        "h" #'idris-docs-at-point))
