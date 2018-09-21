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
