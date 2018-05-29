;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +intero) (load! "+intero"))
      ((featurep! +dante)  (load! "+dante")))


;;
;; Common plugins
;;

(after! haskell-mode
  (set! :repl 'haskell-mode #'switch-to-haskell)
  (add-to-list 'completion-ignored-extensions ".hi"))

