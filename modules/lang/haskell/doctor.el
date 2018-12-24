;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/haskell/doctor.el

(when (featurep! +dante)
  (unless (executable-find "cabal")
    (warn! "Couldn't find cabal, haskell-mode may have issues")))

(when (featurep! +intero)
  (unless (executable-find "stack")
    (warn! "Couldn't find stack. Intero will not work")))

(when (or (featurep! +dante) (featurep! +intero))
  (unless (executable-find "hlint")
    (warn! "Couldn't find hlint. Flycheck may have issues in haskell-mode")))

(when (featurep! +lsp)
  (unless (executable-find "hie")
    (warn! "Couldnt find the Haskell IDE Engine. LSP support will not work.")))
