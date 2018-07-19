;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +intero) (load! "+intero"))
      ((featurep! +dante)  (load! "+dante")))

(when (featurep! :emacs electric)
  (add-hook 'haskell-mode-hook 'electric-indent-local-mode))

;;
;; Common plugins
;;

(def-package! hindent
  :hook (haskell-mode . hindent-mode))

(after! haskell-mode
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (set-lookup-handlers! 'haskell-mode :definition #'haskell-mode-jump-to-def-or-tag)
  (set-file-template! 'haskell-mode :trigger #'haskell-auto-insert-module-template :project t)
  (set-repl-handler! '(haskell-mode haskell-cabal-mode literate-haskell-mode) #'+haskell-repl-buffer)
  (add-to-list 'completion-ignored-extensions ".hi"))

