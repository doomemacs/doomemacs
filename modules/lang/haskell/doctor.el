;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/haskell/doctor.el

(when (featurep! +dante)
  (unless (executable-find "cabal")
    (warn! "Couldn't find cabal, haskell-mode may have issues"))

  (unless (executable-find "ghc-mod")
    (warn! "Couldn't find ghc-mod on PATH. Code completion will not work")))

(when (featurep! +intero)
  (unless (executable-find "stack")
    (warn! "Couldn't find stack. Intero will not work")))

