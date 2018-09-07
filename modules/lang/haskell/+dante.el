;;; lang/haskell/+dante.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dante)

(def-package! dante
  :hook (haskell-mode . dante-mode)
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
  :config
  (when (featurep! :feature syntax-checker)
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

  (map! :map dante-mode-map
        :localleader
        :n "t" #'dante-type-at
        :n "i" #'dante-info
        :n "l" #'haskell-process-load-or-reload
        :n "e" #'dante-eval-block
        :n "a" #'attrap-attrap))
