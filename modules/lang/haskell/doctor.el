;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/haskell/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "cabal")
  (warn! "Couldn't find cabal. haskell-mode may have issues."))

(unless (executable-find "hoogle")
  (warn! "Couldn't find hoogle. Documentation searching will not work."))

(unless (or (featurep! +lsp)
            (executable-find "hlint"))
  (warn! "Couldn't find hlint. Flycheck may have issues in haskell-mode.
  Install it or enable +lsp."))

(when (and (featurep! :editor format)
           (not (featurep! +lsp))
           (not (executable-find "brittany")))
  (warn! "Couldn't find brittany. Code formatting will not work.
  Install it or enable +lsp."))

(when (and (featurep! +lsp)
           (not (executable-find "haskell-language-server-wrapper")))
  (warn! "Couldn't find haskell-language-server."))
