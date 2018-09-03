;;; lang/agda/config.el -*- lexical-binding: t; -*-

(defvar +agda-dir
  (when (executable-find "agda-mode")
    (file-name-directory (shell-command-to-string "agda-mode locate"))))

(def-package! agda-input
  :load-path +agda-dir)

(def-package! agda2-mode
  :mode "\\.agda\\'"
  :after agda-input
  :init
  ;; make syntax-highlighting more consistent with other major modes
   (progn
        (mapc
         (lambda (x) (add-to-list 'face-remapping-alist x))
         '((agda2-highlight-keyword-face                 . font-lock-keyword-face)
           (agda2-highlight-string-face                  . font-lock-string-face)
           (agda2-highlight-number-face                  . font-lock-string-face)
           (agda2-highlight-symbol-face                  . font-lock-variable-name-face)
           (agda2-highlight-primitive-type-face          . font-lock-type-face)
           (agda2-highlight-bound-variable-face          . font-lock-variable-name-face)
           (agda2-highlight-inductive-constructor-face   . font-lock-type-face)
           (agda2-highlight-coinductive-constructor-face . font-lock-type-face)
           (agda2-highlight-datatype-face                . font-lock-type-face)
           (agda2-highlight-field-face                   . font-lock-type-face)
           (agda2-highlight-function-face                . font-lock-function-name-face)
           (agda2-highlight-module-face                  . font-lock-variable-name-face)
           (agda2-highlight-postulate-face               . font-lock-type-face)
           (agda2-highlight-primitive-face               . font-lock-type-face)
           (agda2-highlight-macro-face                   . font-lock-function-name-face)
           (agda2-highlight-record-face                  . font-lock-type-face)
           (agda2-highlight-error-face                   . font-lock-warning-face)
           (agda2-highlight-dotted-face                  . font-lock-variable-name-face)
           (agda2-highlight-unsolved-meta-face           . font-lock-warning-face)
           (agda2-highlight-unsolved-constraint-face     . font-lock-warning-face)
           (agda2-highlight-termination-problem-face     . font-lock-warning-face)
           (agda2-highlight-positivity-problem-face      . font-lock-warning-face)
           (agda2-highlight-incomplete-pattern-face      . font-lock-warning-face)
           (agda2-highlight-typechecks-face              . font-lock-warning-face))))
  :config
  (map! :map agda2-mode-map
        :localleader
        :n "?"  #'agda2-show-goals
        :n "."  #'agda2-goal-and-context-and-inferred
        :n ","   #'agda2-goal-and-context
        :n "="   #'agda2-show-constraints
        :n "SPC" #'agda2-give
        :n "a"   #'agda2-auto
        :n "c"   #'agda2-make-case
        :n "d"   #'agda2-infer-type-maybe-toplevel
        :n "e"   #'agda2-show-context
        :n "gG"  #'agda2-go-back
        :n "h"   #'agda2-helper-function-type
        :n "l"   #'agda2-load
        :n "n"  #'agda2-compute-normalised-maybe-toplevel
        :n "p"  #'agda2-module-contents-maybe-toplevel
        :n "r"  #'agda2-refine
        :n "s"  #'agda2-solveAll
        :n "t"  #'agda2-goal-type
        :n "w"  #'agda2-why-in-scope-maybe-toplevel
        :n "xc" #'agda2-compile
        :n "xd" #'agda2-remove-annotations
        :n "xh" #'agda2-display-implicit-arguments
        :n "xq" #'agda2-quit
        :n "xr" #'agda2-restart))
