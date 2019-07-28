;;; lang/racket/config.el -*- lexical-binding: t; -*-

(use-package! racket-mode
  :hook (racket-repl-mode . racket-unicode-input-method-enable)
  :config
  (set-popup-rule! "^\\*Racket REPL" :size 10 :select t)
  (set-repl-handler! 'racket-mode #'+racket/repl)
  (set-lookup-handlers! 'racket-mode
    :definition    #'racket-visit-definition
    :documentation #'racket-describe)
  (set-docsets! 'racket-mode "Racket")
  (set-pretty-symbols! 'racket-mode
    :lambda  "lambda"
    :map     "map"
    :dot     ".")
  (set-rotate-patterns! 'racket-mode
    :symbols '(("#true" "#false")))

  (add-hook! 'racket-mode-hook
             #'rainbow-delimiters-mode
             #'highlight-quoted-mode)
  (set-lookup-handlers! 'racket-mode :definition #'racket-visit-definition)

  (map! :map (racket-mode-map racket-repl-mode-map)
        :i "[" #'racket-smart-open-bracket)

  (map! :localleader
        :map racket-mode-map
        "a" #'racket-align
        "A" #'racket-unalign
        "f" #'racket-fold-all-tests
        "F" #'racket-unfold-all-tests
        "h" #'racket-doc
        "i" #'racket-unicode-input-method-enable
        "l" #'racket-logger
        "o" #'racket-profile
        "p" #'racket-cycle-paren-shapes
        "r" #'racket-run
        "R" #'racket-run-and-switch-to-repl
        "t" #'racket-test
        "u" #'racket-backward-up-list
        "y" #'racket-insert-lambda
        (:prefix "e"
          "d" #'racket-expand-definition
          "l" #'racket-expand-last-sexp
          "r" #'racket-expand-region
          "a" #'racket-expand-again)
        (:prefix "g"
          "d" #'racket-visit-definition
          "m" #'racket-visit-module
          "r" #'racket-open-require-path
          "b" #'racket-unvisit)
        (:prefix "s"
          "d" #'racket-send-definition
          "l" #'racket-send-last-sexp
          "r" #'racket-send-region)))
