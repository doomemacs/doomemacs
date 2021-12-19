;;; tools/tree-sitter/config.el -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :defer t ;; loading is handled by individual modes
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  ;; This makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t
        ;; and this highlights the entire sub tree in your code
        tree-sitter-debug-highlight-jump-region t))

(defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))
(defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))

(defvar +tree-sitter-keys-mode-map
  (let ((keymap (make-sparse-keymap)))
    (evil-define-key '(visual operator) '+tree-sitter-keys-mode
      "i" +tree-sitter-inner-text-objects-map
      "a" +tree-sitter-outer-text-objects-map)
    keymap)
  "Basic keymap for tree sitter text objects")

(define-minor-mode +tree-sitter-keys-mode
  "A minor mode with tree sitter keybinds."
  :keymap +tree-sitter-keys-mode-map)

(use-package! evil-textobj-tree-sitter
  :when (featurep! :editor evil +everywhere)
  :after tree-sitter
  :config

  (map! (:map +tree-sitter-inner-text-objects-map
         "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
         "F" (evil-textobj-tree-sitter-get-textobj "call.inner")
         "C" (evil-textobj-tree-sitter-get-textobj "class.inner")
         "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
         "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
        (:map +tree-sitter-outer-text-objects-map
         "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
         "F" (evil-textobj-tree-sitter-get-textobj "call.outer")
         "C" (evil-textobj-tree-sitter-get-textobj "class.outer")
         "c" (evil-textobj-tree-sitter-get-textobj "comment.outer")
         "i" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
         "l" (evil-textobj-tree-sitter-get-textobj "loop.outer")))

  (after! which-key
    (setq which-key-allow-multiple-replacements t)
    (pushnew!
     which-key-replacement-alist
     '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1")))))
