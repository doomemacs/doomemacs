;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +intero) (load! "+intero"))
      ((featurep! +dante)  (load! "+dante"))
      ((featurep! +lsp)    (load! "+lsp")))


;;
;; Common plugins
;;

(after! haskell-mode
  (set-repl-handler! 'haskell-mode #'switch-to-haskell)
  (add-to-list 'completion-ignored-extensions ".hi"))
