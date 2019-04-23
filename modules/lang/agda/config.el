;;; lang/agda/config.el -*- lexical-binding: t; -*-

(defvar +agda-dir
  (when (executable-find "agda-mode")
    (file-name-directory (shell-command-to-string "agda-mode locate"))))

(def-package! agda2
  :when +agda-dir
  :load-path +agda-dir)

(def-package! agda2-mode
  :defer t
  :config
  (map! :map agda2-mode-map
        :localleader
        "?"   #'agda2-show-goals
        "."   #'agda2-goal-and-context-and-inferred
        ","   #'agda2-goal-and-context
        "="   #'agda2-show-constraints
        "SPC" #'agda2-give
        "a"   #'agda2-auto
        "c"   #'agda2-make-case
        "d"   #'agda2-infer-type-maybe-toplevel
        "e"   #'agda2-show-context
        "gG"  #'agda2-go-back
        "h"   #'agda2-helper-function-type
        "l"   #'agda2-load
        "n"   #'agda2-compute-normalised-maybe-toplevel
        "p"   #'agda2-module-contents-maybe-toplevel
        "r"   #'agda2-refine
        "s"   #'agda2-solveAll
        "t"   #'agda2-goal-type
        "w"   #'agda2-why-in-scope-maybe-toplevel
        (:prefix "x"
          "c"   #'agda2-compile
          "d"   #'agda2-remove-annotations
          "h"   #'agda2-display-implicit-arguments
          "q"   #'agda2-quit
          "r"   #'agda2-restart)))
