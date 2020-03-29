;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "stack.yaml"))


;;
;;; Common packages

(after! haskell-mode
  (setq haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
        haskell-process-auto-import-loaded-modules t
        haskell-process-show-overlays (not (featurep! :checkers syntax))) ; redundant with flycheck

  (set-lookup-handlers! 'haskell-mode
    :definition #'haskell-mode-jump-to-def-or-tag)
  (set-file-template! 'haskell-mode
    :trigger #'haskell-auto-insert-module-template
    :project t)
  (set-repl-handler!
    '(haskell-mode haskell-cabal-mode literate-haskell-mode)
    #'+haskell/open-repl :persist t)
  ;; Don't kill REPL popup on ESC/C-g
  (set-popup-rule! "^\\*haskell\\*" :quit nil)

  (add-hook! 'haskell-mode-hook
             #'haskell-collapse-mode ; support folding haskell code blocks
             #'interactive-haskell-mode)

  (add-to-list 'completion-ignored-extensions ".hi")

  (map! :map haskell-mode-map
        :n "o" #'+haskell/evil-open-below
        :n "O" #'+haskell/evil-open-above)

  (map! :localleader
        :map haskell-mode-map
        ;; this is set to use cabal for dante users and stack for intero users:
        "b" #'haskell-process-cabal-build
        "c" #'haskell-cabal-visit-file
        "h" #'haskell-hide-toggle
        "H" #'haskell-hide-toggle-all))


;;
;;; Backends

(cond ((featurep! +intero) (load! "+intero")) ; DEPRECATED
      ((featurep! +dante)  (load! "+dante"))
      ((or (featurep! +lsp)
           (featurep! +ghcide))
       (load! "+lsp")))
