;;; lang/factor/config.el -*- lexical-binding: t; -*-

(use-package! fuel-mode
  :defer t
  :init
  (after! factor-mode
    (set-eval-handler! 'factor-mode #'fuel-eval-region)
    (set-repl-handler! 'factor-mode #'run-factor))
  :config
  (set-lookup-handlers! 'factor-mode
    :definition #'fuel-edit-word-at-point
    :references #'fuel-show-callers
    :documentation #'fuel-help))

(map! :after factor-mode
      :map factor-mode-map
      :localleader
      "t" #'fuel-test-vocab
      (:prefix ("e" . "eval")
        "d" #'fuel-eval-definition
        "R" #'fuel-eval-extended-region
        "r" #'fuel-eval-region)
      (:prefix ("h" . "help")
        "p" #'fuel-apropos
        "v" #'fuel-show-file-words
        "c" #'fuel-show-callees
        "e" #'fuel-stack-effect-region)
      (:prefix ("s" . "scaffold")
        "v" #'fuel-scaffold-vocab
        "h" #'fuel-scaffold-help
        "t" #'fuel-scaffold-tests)
      (:prefix ("r" . "refactor")
        "s" #'fuel-refactor-extract-sexp
        "w" #'fuel-refactor-extract-region
        "v" #'fuel-refactor-extract-vocab
        "i" #'fuel-refactor-inline-word
        "g" #'fuel-refactor-make-generic
        "u" #'fuel-update-usings
        "r" #'fuel-refactor-rename-word))

(map! :after fuel-listener
      :map fuel-listener-mode-map
      :localleader
      "e" #'fuel-edit-vocabulary
      "r" #'fuel-refresh-all
      "i" #'fuel-stack-mode
      "h" #'fuel-help
      "s" #'fuel-scaffold-vocab)
