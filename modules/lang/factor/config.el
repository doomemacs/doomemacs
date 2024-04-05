;;; lang/factor/config.el -*- lexical-binding: t; -*-

(use-package! fuel-mode
  :defer t
  :init
  (after! factor-mode
    (set-eval-handler! 'factor-mode #'fuel-eval-region)
    (set-repl-handler! 'factor-mode #'+factor/open-repl
      :persist t
      :send-region #'fuel-eval-region
      :send-buffer #'fuel-run-file)
    (set-lookup-handlers! 'factor-mode
      :definition #'fuel-edit-word-at-point
      :references #'fuel-show-callers
      :documentation #'fuel-help)
    (map! :map factor-mode-map
          :localleader
          "t" #'fuel-test-vocab
          "F" #'fuel-run-file
          "f" #'run-factor
          "a" #'fuel-refresh-all
          "L" #'fuel-load-usings
          "u" #'fuel-vocab-usage
          "U" #'fuel-vocab-uses
          (:prefix ("c" . "change")
           "w" #'fuel-edit-word-at-point
           "d" #'fuel-edit-word-doc-at-point
           "v" #'fuel-edit-vocabulary)
          (:prefix ("e" . "eval")
           "d" #'fuel-eval-definition
           "R" #'fuel-eval-extended-region
           "r" #'fuel-eval-region)
          (:prefix ("h" . "help")
           "p" #'fuel-apropos
           "h" #'fuel-help
           "b" #'fuel-help-display-bookmarks
           "v" #'fuel-help-vocab
           "w" #'fuel-show-file-words
           "c" #'fuel-show-callees
           "e" #'fuel-stack-effect-region
           "s" #'fuel-stack-effect-sexp)
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
           "r" #'fuel-refactor-rename-word)))
  :config
  (set-popup-rules!
    '(("^\\*fuel \\(debug\\|xref\\|messages\\)\\*$" :slot 1 :vslot -1)
      ("^\\*fuel help\\*$" :slot 2 :vslot 2 :select t :size 0.35)
      ("^\\*fuel listener\\*$" :size 0.3 :quit nil :ttl nil)))

  (map! :after fuel-help
        :map fuel-help-mode-map
        :localleader
        "e" #'fuel-help-edit
        "d" #'fuel-help-delete-bookmark
        "B" #'fuel-help-display-bookmarks
        "n" #'fuel-help-next
        "d" #'fuel-help-kill-page
        "p" #'fuel-help-previous
        "b" #'fuel-help-bookmark-page
        "e" #'fuel-help-edit)

  (map! :after fuel-listener
        :map fuel-listener-mode-map
        :localleader
        "b" #'fuel-switch-to-buffer
        "w" #'fuel-switch-to-buffer-other-window
        "f" #'fuel-switch-to-buffer-other-frame
        "e" #'fuel-edit-vocabulary
        "r" #'fuel-refresh-all
        "i" #'fuel-stack-mode
        "h" #'fuel-help
        "s" #'fuel-scaffold-vocab))

