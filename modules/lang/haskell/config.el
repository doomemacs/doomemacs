;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +intero) (load! "+intero"))
      ((featurep! +dante)  (load! "+dante")))

;;
;; Common plugins
;;

(def-package! hindent
  :hook (haskell-mode . hindent-mode))

(after! haskell-mode
  (map! :map haskell-mode-map
        :localleader
        :n "p" #'hindent-reformat-buffer
        :v "p" #'hindent-reformat-region))

(after! intero-mode
  (map! :map intero-mode-map
        :localleader
        :n "t" #'intero-type-at
        :n "i" #'intero-info
        :n "l" #'intero-repl-load
        :n "e" #'intero-repl-eval-region
        :v "e" #'intero-repl-eval-region
        :n "a" #'intero-apply-suggestions))

(after! dante-mode
  (map! :map dante-mode-map
        :localleader
        :n "b" #'haskell-process-cabal-build
        :n "c" #'haskell-cabal-visit-file
        :n "t" #'dante-type-at
        :n "i" #'dante-info
        :n "l" #'haskell-process-load-or-reload
        :n "e" #'dante-eval-block
        :n "a" #'attrap-attrap))

(after! haskell-mode
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (set-lookup-handlers! 'haskell-mode :definition #'haskell-mode-jump-to-def-or-tag)
  (set-file-template! 'haskell-mode :trigger #'haskell-auto-insert-module-template :project t)
  (set-repl-handler! '(haskell-mode haskell-cabal-mode literate-haskell-mode) #'+haskell-repl-buffer)

  (add-to-list 'completion-ignored-extensions ".hi"))

