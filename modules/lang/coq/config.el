;;; lang/coq/config.el -*- lexical-binding: t; -*-

;; `coq'
(setq proof-electric-terminator-enable t)

;; We've replaced coq-mode abbrevs with yasnippet snippets (in the snippets
;; library included with Doom).
(setq coq-mode-abbrev-table '())

(map! :after coq-mode
      :map coq-mode-map
      :localleader
      "]"  #'proof-assert-next-command-interactive
      "["  #'proof-undo-last-successful-command
      "."  #'proof-goto-point
      (:prefix ("l" . "layout")
        "c" #'pg-response-clear-displays
        "l" #'proof-layout-windows
        "p" #'proof-prf)
      (:prefix ("p" . "proof")
        "i" #'proof-interrupt-process
        "p" #'proof-process-buffer
        "q" #'proof-shell-exit
        "r" #'proof-retract-buffer)
      (:prefix ("a" . "about/print/check")
        "a" #'coq-Print
        "A" #'coq-Print-with-all
        "b" #'coq-About
        "B" #'coq-About-with-all
        "c" #'coq-Check
        "C" #'coq-Check-show-all
        "f" #'proof-find-theorems
        (:prefix ("i" . "implicits")
          "b" #'coq-About-with-implicits
          "c" #'coq-Check-show-implicits
          "i" #'coq-Print-with-implicits))
      (:prefix ("g" . "goto")
        "e" #'proof-goto-command-end
        "l" #'proof-goto-end-of-locked
        "s" #'proof-goto-command-start)
      (:prefix ("i" . "insert")
        "c" #'coq-insert-command
        "e" #'coq-end-Section
        "i" #'coq-insert-intros
        "r" #'coq-insert-requires
        "s" #'coq-insert-section-or-module
        "t" #'coq-insert-tactic
        "T" #'coq-insert-tactical))

(after! company-coq
  (set-popup-rule! "^\\*\\(?:response\\|goals\\)\\*" :ignore t)
  (set-lookup-handlers! 'company-coq-mode
    :definition #'company-coq-jump-to-definition
    :references #'company-coq-grep-symbol
    :documentation #'company-coq-doc)
  (unless (featurep! :completion company)
    (setq company-coq-disabled-features '(company company-defaults)))

  (map! :map coq-mode-map
        :localleader
        (:prefix ("i" . "insert")
          "l" #'company-coq-lemma-from-goal
          "m" #'company-coq-insert-match-construct)
        "ao" #'company-coq-occur
        (:prefix ("h" . "help")
          "e" #'company-coq-document-error
          "E" #'company-coq-browse-error-messages
          "h" #'company-coq-doc)))
