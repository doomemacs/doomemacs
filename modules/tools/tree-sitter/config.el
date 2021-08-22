;;; tools/tree-sitter/config.el -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :when (bound-and-true-p module-file-suffix)
  :hook (prog-mode . turn-on-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)

  ;; This makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t
        ;; and this highlights the entire sub tree in your code
        tree-sitter-debug-highlight-jump-region t))

(when (featurep! :editor evil +everywhere)
  (use-package! evil-textobj-tree-sitter
    :after tree-sitter
    :config
    (map!
     :textobj "f" nil nil
     :textobj "f" (evil-textobj-tree-sitter-get-textobj "function.inner") (evil-textobj-tree-sitter-get-textobj "function.outer") ;; redef

     :textobj "C" (evil-textobj-tree-sitter-get-textobj "class.inner") (evil-textobj-tree-sitter-get-textobj "class.outer")

     :textobj "c" nil nil
     :textobj "c" nil (evil-textobj-tree-sitter-get-textobj "comment.outer")

     :textobj "i" nil nil
     :textobj "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner") (evil-textobj-tree-sitter-get-textobj "conditional.outer")

     :textobj "l" nil nil
     :textobj "l" (evil-textobj-tree-sitter-get-textobj "loop.inner") (evil-textobj-tree-sitter-get-textobj "loop.outer"))))
