;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/haskell/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "cabal")
  (warn! "Couldn't find cabal. haskell-mode may have issues."))

(unless (executable-find "hoogle")
  (warn! "Couldn't find hoogle. Documentation searching will not work."))

(unless (or (modulep! +lsp)
            (executable-find "hlint"))
  (warn! "Couldn't find hlint. Flycheck may have issues in haskell-mode.
  Install it or enable +lsp."))

(when (and (modulep! :editor format)
           (modulep! -lsp)
           (not (executable-find "brittany")))
  (warn! "Couldn't find brittany. Code formatting will not work.
  Install it or enable +lsp."))

(when (and (modulep! +lsp)
           (not (executable-find "haskell-language-server-wrapper")))
  (warn! "Couldn't find haskell-language-server."))
