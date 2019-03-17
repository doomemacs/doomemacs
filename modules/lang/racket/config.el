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
        (:prefix ("m" . "macro")
          "d" #'racket-expand-definition
          "e" #'racket-expand-last-sexp
          "r" #'racket-expand-region)
        (:prefix ("g" . "goto")
          "b" #'racket-unvisit
          "d" #'racket-visit-definition
          "m" #'racket-visit-module
          "r" #'racket-open-require-path)
        (:prefix "s"
          "d" #'racket-send-definition
          "l" #'racket-send-last-sexp
          "r" #'racket-send-region)))
