;;; tools/tree-sitter/config.el -*- lexical-binding: t; -*-



(use-package! tree-sitter
  ;; :hook (prog-mode . turn-on-tree-sitter-mode)
  :defer t ;; loading is handled by individual modes
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (defvar +tree-sitter-enabled-mode-maps (seq-map (lambda (mode)
                                                     (intern (concat
                                                              (symbol-name (car mode)) "-map")))
                                                   tree-sitter-major-mode-language-alist)
    "List of mode hooks for tree sitter enabled modes.")
  ;; This makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t
        ;; and this highlights the entire sub tree in your code
        tree-sitter-debug-highlight-jump-region t))

(if (daemonp) ;; eager load when in daemon as its start time is easily consumed
    (require 'tree-sitter-langs)
  (add-hook! 'tree-sitter-after-on-hook
    (require 'tree-sitter-langs)))


(when (featurep! :editor evil +everywhere)
  (use-package! evil-textobj-tree-sitter
    :after tree-sitter
    :config
    (map!
     :map +tree-sitter-enabled-mode-maps
     :textobj "f" nil nil
     :textobj "f" (evil-textobj-tree-sitter-get-textobj "function.inner") (evil-textobj-tree-sitter-get-textobj "function.outer")

     :textobj "F" (evil-textobj-tree-sitter-get-textobj "call.inner") (evil-textobj-tree-sitter-get-textobj "call.outer")

     :textobj "C" (evil-textobj-tree-sitter-get-textobj "class.inner") (evil-textobj-tree-sitter-get-textobj "class.outer")

     :textobj "c" nil nil
     :textobj "c" nil (evil-textobj-tree-sitter-get-textobj "comment.outer")

     :textobj "i" nil nil
     :textobj "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner") (evil-textobj-tree-sitter-get-textobj "conditional.outer")

     :textobj "l" nil nil
     :textobj "l" (evil-textobj-tree-sitter-get-textobj "loop.inner") (evil-textobj-tree-sitter-get-textobj "loop.outer"))
    (after! which-key
      (setq which-key-allow-multiple-replacements t)
      (pushnew!
       which-key-replacement-alist
       '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1"))))))
