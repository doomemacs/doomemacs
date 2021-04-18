;;; lang/fstar/config.el -*- lexical-binding: t; -*-

(after! fstar-mode
  (set-lookup-handlers! 'fstar-mode
    :definition #'fstar-jump-to-definition
    :documentation #'fstar-doc-at-point-dwim)

  (map! :map fstar-mode-map
        :localleader
        :desc "F* next" "]" #'fstar-subp-advance-next
        :desc "F* go to point" "." #'fstar-subp-advance-or-retract-to-point
        :desc "F* previous" "[" #'fstar-subp-retract-last
        (:prefix ("p" . "proof")
          :desc "go to point (lax)" "l" #'fstar-subp-advance-or-retract-to-point-lax
          :desc "compile buffer (lax)" "b" #'fstar-subp-advance-to-point-max-lax
          "q" #'fstar-subp-kill-one-or-many
          "k" #'fstar-subp-kill-z3
          "r" #'fstar-subp-reload-to-point)

        (:prefix ("l" . "layout")
          "c"  #'fstar-quit-windows
          "o"  #'fstar-outline)

        ;; Moving around
        "'" #'fstar-jump-to-related-error
        (:prefix ("j" . "jump")
          "j" #'fstar-jump-to-definition
          "f" #'fstar-jump-to-definition-other-frame
          "w" #'fstar-jump-to-definition-other-window
          "e" #'fstar-jump-to-related-error
          "F" #'fstar-jump-to-related-error-other-frame
          "W" #'fstar-jump-to-related-error-other-window
          "d" #'fstar-visit-dependency
          "a" #'fstar-visit-interface-or-implementation
          :desc "jump to first unprocessed line" "u" #'fstar-subp-goto-beginning-of-unprocessed)

        ;; Help !!!
        (:prefix ("h" . "help")
          "y" #'fstar-copy-help-at-point
          "w" #'fstar-browse-wiki
          "W" #'fstar-browse-wiki-in-browser
          "o" #'fstar-list-options
          "p" #'fstar-quick-peek)

        (:prefix ("a" . "ask (queries)")
          "a" #'fstar-print
          "e" #'fstar-eval
          "E" #'fstar-eval-custom
          "s" #'fstar-search
          "d" #'fstar-doc)

        (:prefix ("i" . "insert")
          "m" #'fstar-insert-match-dwim
          "M" #'fstar-insert-match)
        ))
