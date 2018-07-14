;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +intero) (load! "+intero"))
      ((featurep! +dante)  (load! "+dante")))

(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(after! haskell-mode
  (set-repl-handler! 'haskell-mode #'haskell-interactive-bring)
  (set-repl-handler! 'haskell-cabal-mode #'haskell-interactive-bring)
  (set-repl-handler! 'literate-haskell-mode #'haskell-interactive-bring)
  (add-to-list 'completion-ignored-extensions ".hi"))

;;
;; Common plugins
;;

(def-package! hindent
  :hook (haskell-mode . hindent-mode))
