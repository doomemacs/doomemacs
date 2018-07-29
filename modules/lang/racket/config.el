;;; lang/racket/config.el -*- lexical-binding: t; -*-

(def-package! racket-mode
  :hook ((racket-mode racket-repl-mode) . racket-unicode-input-method-enable)
  :config
  (set-popup-rule! "^\\*Racket REPL" :size 10 :select t)
  (set-repl-handler! 'racket-mode #'+racket/repl)
  (set-company-backend! 'racket-mode
    '(company-abbrev company-dabbrev-code company-dabbrev company-files))
  (set-lookup-handlers! 'racket-mode
    :definition    #'racket-describe
    :documentation #'racket-doc)
  (set-docset! 'racket-mode "Racket")
  (set-pretty-symbols! 'racket-mode
    :lambda  "lambda"
    :map     "map"
    :dot     ".")
  (set-rotate-patterns! 'racket-mode
    :symbols '(("#true" "#false")))

  (setq racket-smart-open-bracket-enable t)

  (add-hook! racket-mode
    #'(;; 3rd-party functionality
       doom|enable-delete-trailing-whitespace
       ;; fontification
       rainbow-delimiters-mode
       highlight-quoted-mode))

  (map! :map racket-mode-map
        :localleader
        :n "c"   #'racket-run
        :n "z"   #'racket-repl
        :n "C"   #'racket-run-and-switch-to-repl
        :n "x"   #'racket-racket
        :n "t"   #'racket-test
        :n "l"   #'racket-logger
        :n "o"   #'racket-profile
        (:desc "eval" :prefix "s"
          :n "d" #'racket-send-definition
          :n "l" #'racket-send-last-sexp
          :n "r" #'racket-send-region)
        (:desc "macro expand" :prefix "e"
          :n "d" #'racket-expand-definition
          :n "l" #'racket-expand-last-sexp
          :n "r" #'racket-expand-region
          :n "a" #'racket-expand-again)
        :n "r"   #'racket-open-require-path
        :n "TAB" #'indent-for-tab-command
        :n "u"   #'racket-backward-up-list
        :n "["   #'racket-smart-open-bracket
        :n ")"   #'racket-insert-closing
        :n "]"   #'racket-insert-closing
        :n "}"   #'racket-insert-closing
        :n "p"   #'racket-cycle-paren-shapes
        :n "y"   #'racket-insert-lambda
        :n "d"   #'racket-doc
        :n "."   #'racket-describe
        :n "M-." #'racket-visit-definition
        :n "C-." #'racket-visit-module
        :n ","   #'racket-unvisit
        :n "f"   #'racket-fold-all-tests
        :n "F"   #'racket-unfold-all-tests
        :n "a"   #'racket-align
        :n "A"   #'racket-unalign
        :nv ";"  #'comment-dwim
        :nv "\\" #'indent-region))
