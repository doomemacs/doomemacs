;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(cond ((featurep! +intero) (load! "+intero"))
      ((featurep! +dante)  (load! "+dante")))

;;
;; Common plugins
;;

(def-package! hindent
  :hook (haskell-mode . hindent-mode))

(after! haskell-mode
  (setq haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
        haskell-process-auto-import-loaded-modules t)
  (when (featurep! :feature syntax-checker)
    (setq haskell-process-show-overlays nil))  ; flycheck makes this unnecessary
  (add-hook! 'haskell-mode-hook 
    #'(subword-mode           ; improves text navigation with camelCase
       haskell-collapse-mode  ; support folding haskell code blocks
       interactive-haskell-mode))
  (set-lookup-handlers! 'haskell-mode :definition #'haskell-mode-jump-to-def-or-tag)
  (set-file-template! 'haskell-mode :trigger #'haskell-auto-insert-module-template :project t)
  (set-repl-handler! '(haskell-mode haskell-cabal-mode literate-haskell-mode) #'+haskell-repl-buffer)

  (add-to-list 'completion-ignored-extensions ".hi")

  (map! :map haskell-mode-map
        :localleader
        ;; this is set to use cabal for dante users and stack for intero users:
        :n "b" #'haskell-process-cabal-build
        :n "c" #'haskell-cabal-visit-file
        :n "p" #'hindent-reformat-buffer
        :v "p" #'hindent-reformat-region
        :v "h" #'haskell-hide-toggle
        :nv "H" #'haskell-hide-toggle-all))

