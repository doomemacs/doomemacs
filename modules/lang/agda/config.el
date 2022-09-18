;;; lang/agda/config.el -*- lexical-binding: t; -*-

(when (and (modulep! +local)
           (executable-find "agda-mode"))
  (add-load-path!
   (file-name-directory (shell-command-to-string "agda-mode locate")))
  (unless (require 'agda2 nil t)
    (message "Failed to find the `agda2' package")))

(after! agda2-mode
  (set-lookup-handlers! 'agda2-mode
    :definition #'agda2-goto-definition-keyboard)

  (when (modulep! +tree-sitter)
    (set-tree-sitter-lang! 'agda2-mode 'agda)
    (add-hook! '(agda-mode-local-vars-hook
                 agda2-mode-local-vars-hook)
               :append #'tree-sitter!))

  (map! :map agda2-mode-map
        :localleader
        "?"   #'agda2-show-goals
        "."   #'agda2-goal-and-context-and-inferred
        ","   #'agda2-goal-and-context
        "="   #'agda2-show-constraints
        "SPC" #'agda2-give
        "a"   #'agda2-auto-maybe-all
        "b"   #'agda2-previous-goal
        "c"   #'agda2-make-case
        "d"   #'agda2-infer-type-maybe-toplevel
        "e"   #'agda2-show-context
        "f"   #'agda2-next-goal
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
