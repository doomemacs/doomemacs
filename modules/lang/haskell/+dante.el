;;; lang/haskell/+dante.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dante)

(def-package! dante
  :hook (haskell-mode . dante-mode))
