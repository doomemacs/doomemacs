;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +intero) (load! "+intero"))
      ((featurep! +dante)  (load! "+dante")))

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

  (map! :map haskell-mode-map
        :localleader
        :n "p" #'hindent-reformat-buffer
        :v "p" #'hindent-reformat-region
        (:when (featurep! +intero)
          :desc "type" :n "t" #'intero-type-at
          :desc "info" :n "i" #'intero-info
          :desc "load" :n "l" #'intero-repl-load
          :desc "eval line" :n "e" #'intero-repl-eval-region
          :desc "eval region" :v "e" #'intero-repl-eval-region
          :desc "apply suggestions" :n "a" #'intero-apply-suggestions)
        (:when (featurep! +dante)
          :n "n" "b" #'haskell-process-cabal-build
          :desc "goto cabal file" :n "c" #'haskell-cabal-visit-file
          :desc "type" :n "t" #'dante-type-at
          :desc "info" :n "i" #'dante-info
          :desc "load" :n "l" #'haskell-process-load-or-reload
          :desc "eval command block >>>" :n "e" #'dante-eval-block
          :desc "repair at point" :n "a" #'attrap-attrap))

  (add-to-list 'completion-ignored-extensions ".hi"))

