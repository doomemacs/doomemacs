;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "stack.yaml"))


;;
;;; Common packages

(after! haskell-mode
  (setq haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
        haskell-process-auto-import-loaded-modules t
        haskell-process-show-overlays (not (modulep! :checkers syntax))) ; redundant with flycheck

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

  (when (modulep! +tree-sitter)
    (add-hook 'haskell-mode-local-vars-hook #'tree-sitter! 'append))

  (add-to-list 'completion-ignored-extensions ".hi")

  (map! :map haskell-mode-map
        :n "o" #'+haskell/evil-open-below
        :n "O" #'+haskell/evil-open-above
        (:when (modulep! :tools lookup)
         [remap haskell-mode-jump-to-def-or-tag] #'+lookup/definition))

  (map! :localleader
        :map haskell-mode-map
        "b" #'haskell-process-cabal-build
        "c" #'haskell-cabal-visit-file
        "h" #'haskell-hide-toggle
        "H" #'haskell-hide-toggle-all))


(use-package! lsp-haskell
  :when (modulep! +lsp)
  :defer t
  :init
  (add-hook 'haskell-mode-local-vars-hook #'lsp! 'append)
  (add-hook 'haskell-literate-mode-local-vars-hook #'lsp! 'append)
  (after! lsp-mode (require 'lsp-haskell))
  :config
  ;; Does some strange indentation if it pastes in the snippet
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed))
