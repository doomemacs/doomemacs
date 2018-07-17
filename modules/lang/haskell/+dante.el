;;; lang/haskell/+dante.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dante)

(def-package! dante
  :init
  (setq dante-load-flags '(;; defaults:
                           "+c"
                           "-Wwarn=missing-home-modules"
                           "-fno-diagnostics-show-caret"
                           ;; neccessary to make attrap-attrap useful:
                           "-Wall"
                           ;; necessary to make company completion useful:
                           "-fdefer-typed-holes"
                           "-fdefer-type-errors"))
  :hook (haskell-mode . dante-mode))

(def-package! attrap
  :commands (attrap-attrap))
