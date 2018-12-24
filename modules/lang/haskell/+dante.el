;;; lang/haskell/+dante.el -*- lexical-binding: t; -*-
;;;###if (featurep! +dante)

(def-package! dante
  :hook (haskell-mode-local-vars . dante-mode)
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

  (when (featurep 'evil)
    (add-hook 'dante-mode-hook #'evil-normalize-keymaps))
  (map! :map dante-mode-map
        :localleader
        "t" #'dante-type-at
        "i" #'dante-info
        "l" #'haskell-process-load-or-reload
        "e" #'dante-eval-block
        "a" #'attrap-attrap))
