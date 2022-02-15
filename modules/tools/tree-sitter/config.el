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
(defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
(defvar +tree-sitter-goto-next-map (make-sparse-keymap))

(defvar +tree-sitter-keys-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;; ts text objects
    (evil-define-key '(visual operator) '+tree-sitter-keys-mode
      "i" +tree-sitter-inner-text-objects-map
      "a" +tree-sitter-outer-text-objects-map)
    ;; ts goto nodes
    (evil-define-key 'normal '+tree-sitter-keys-mode
      "[g" +tree-sitter-goto-previous-map
      "]g" +tree-sitter-goto-next-map)
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
         "A" (evil-textobj-tree-sitter-get-textobj ("parameter.inner" "call.inner"))
         "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
         "F" (evil-textobj-tree-sitter-get-textobj "call.inner")
         "C" (evil-textobj-tree-sitter-get-textobj "class.inner")
         "v" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
         "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
        (:map +tree-sitter-outer-text-objects-map
         "A" (evil-textobj-tree-sitter-get-textobj ("parameter.outer" "call.outer"))
         "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
         "F" (evil-textobj-tree-sitter-get-textobj "call.outer")
         "C" (evil-textobj-tree-sitter-get-textobj "class.outer")
         "c" (evil-textobj-tree-sitter-get-textobj "comment.outer")
         "v" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
         "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))

        (:map +tree-sitter-goto-previous-map
         "a" (+tree-sitter-goto-textobj "parameter.outer" t)
         "f" (+tree-sitter-goto-textobj "function.outer" t)
         "F" (+tree-sitter-goto-textobj "call.outer" t)
         "C" (+tree-sitter-goto-textobj "class.outer" t)
         "c" (+tree-sitter-goto-textobj "comment.outer" t)
         "i" (+tree-sitter-goto-textobj "conditional.outer" t)
         "l" (+tree-sitter-goto-textobj "loop.outer" t))
        (:map +tree-sitter-goto-next-map
         "a" (+tree-sitter-goto-textobj "parameter.outer")
         "f" (+tree-sitter-goto-textobj "function.outer")
         "F" (+tree-sitter-goto-textobj "call.outer")
         "C" (+tree-sitter-goto-textobj "class.outer")
         "c" (+tree-sitter-goto-textobj "comment.outer")
         "i" (+tree-sitter-goto-textobj "conditional.outer")
         "l" (+tree-sitter-goto-textobj "loop.outer")))


  (after! which-key
    (setq which-key-allow-multiple-replacements t)
    (pushnew!
     which-key-replacement-alist
     '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1")))))
