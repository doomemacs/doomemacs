;;; lang/racket/config.el -*- lexical-binding: t; -*-

(def-package! racket-mode
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

  (setq racket-smart-open-bracket-enable t)

  (add-hook! racket-mode #'(rainbow-delimiters-mode highlight-quoted-mode))

  (map! :map racket-mode-map
        :localleader
        :n "a"   #'racket-align
        :n "A"   #'racket-unalign
        :n "f"   #'racket-fold-all-tests
        :n "F"   #'racket-unfold-all-tests
        :n "h"   #'racket-doc
        :n "i"   #'racket-unicode-input-method-enable
        :n "l"   #'racket-logger
        :n "o"   #'racket-profile
        :n "p"   #'racket-cycle-paren-shapes
        :n "r"   #'racket-run
        :n "R"   #'racket-run-and-switch-to-repl
        :n "t"   #'racket-test
        :n "u"   #'racket-backward-up-list
        :n "y"   #'racket-insert-lambda
        (:prefix "e"
          :n "d" #'racket-expand-definition
          :n "l" #'racket-expand-last-sexp
          :n "r" #'racket-expand-region
          :n "a" #'racket-expand-again)
        (:prefix "g"
          :n "d" #'racket-visit-definition
          :n "m" #'racket-visit-module
          :n "r" #'racket-open-require-path
          :n "b" #'racket-unvisit)
        (:prefix "s"
          :n "d" #'racket-send-definition
          :n "l" #'racket-send-last-sexp
          :n "r" #'racket-send-region)))
