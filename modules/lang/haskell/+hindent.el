;;; +hindent.el --- description -*- lexical-binding: t; -*-
;;;###if (featurep! +hindent)

(def-package! hindent
  :hook (haskell-mode . hindent-mode))

