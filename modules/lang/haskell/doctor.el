;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/haskell/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(when (featurep! +dante)
  (unless (executable-find "cabal")
    (warn! "Couldn't find cabal, haskell-mode may have issues")))

(when (featurep! +intero)
  (unless (executable-find "stack")
    (warn! "Couldn't find stack. Intero will not work")))

(when (or (featurep! +dante) (featurep! +intero))
  (unless (executable-find "hlint")
    (warn! "Couldn't find hlint. Flycheck may have issues in haskell-mode")))
