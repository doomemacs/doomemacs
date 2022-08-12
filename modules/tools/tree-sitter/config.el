;;; tools/tree-sitter/config.el -*- lexical-binding: t; -*-

(defvar +tree-sitter-hl-enabled-modes '(not web-mode typescript-tsx-mode)
  "A list of major modes which should be highlighted by tree-sitter.

If this list begins with `not', then it negates the list.
If it is t, it is enabled in all modes.
If nil, it is disabled in all modes")

;;
;;; Packages

(use-package! tree-sitter
  :defer t
  :config
  (require 'tree-sitter-langs)
  ;; This makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t
        ;; and this highlights the entire sub tree in your code
        tree-sitter-debug-highlight-jump-region t))


(use-package! evil-textobj-tree-sitter
  :when (modulep! :editor evil +everywhere)
  :defer t
  :init (after! tree-sitter (require 'evil-textobj-tree-sitter))
  :config
  (defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))
  (defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))
  (defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
  (defvar +tree-sitter-goto-next-map (make-sparse-keymap))

  (evil-define-key '(visual operator) 'tree-sitter-mode
    "i" +tree-sitter-inner-text-objects-map
    "a" +tree-sitter-outer-text-objects-map)
  (evil-define-key 'normal 'tree-sitter-mode
    "[g" +tree-sitter-goto-previous-map
    "]g" +tree-sitter-goto-next-map)

  (map! (:map +tree-sitter-inner-text-objects-map
         "A" (+tree-sitter-get-textobj '("parameter.inner" "call.inner"))
         "f" (+tree-sitter-get-textobj "function.inner")
         "F" (+tree-sitter-get-textobj "call.inner")
         "C" (+tree-sitter-get-textobj "class.inner")
         "v" (+tree-sitter-get-textobj "conditional.inner")
         "l" (+tree-sitter-get-textobj "loop.inner"))
        (:map +tree-sitter-outer-text-objects-map
         "A" (+tree-sitter-get-textobj '("parameter.outer" "call.outer"))
         "f" (+tree-sitter-get-textobj "function.outer")
         "F" (+tree-sitter-get-textobj "call.outer")
         "C" (+tree-sitter-get-textobj "class.outer")
         "c" (+tree-sitter-get-textobj "comment.outer")
         "v" (+tree-sitter-get-textobj "conditional.outer")
         "l" (+tree-sitter-get-textobj "loop.outer"))

        (:map +tree-sitter-goto-previous-map
         "a" (+tree-sitter-goto-textobj "parameter.outer" t)
         "f" (+tree-sitter-goto-textobj "function.outer" t)
         "F" (+tree-sitter-goto-textobj "call.outer" t)
         "C" (+tree-sitter-goto-textobj "class.outer" t)
         "c" (+tree-sitter-goto-textobj "comment.outer" t)
         "v" (+tree-sitter-goto-textobj "conditional.outer" t)
         "l" (+tree-sitter-goto-textobj "loop.outer" t))
        (:map +tree-sitter-goto-next-map
         "a" (+tree-sitter-goto-textobj "parameter.outer")
         "f" (+tree-sitter-goto-textobj "function.outer")
         "F" (+tree-sitter-goto-textobj "call.outer")
         "C" (+tree-sitter-goto-textobj "class.outer")
         "c" (+tree-sitter-goto-textobj "comment.outer")
         "v" (+tree-sitter-goto-textobj "conditional.outer")
         "l" (+tree-sitter-goto-textobj "loop.outer")))

  (after! which-key
    (setq which-key-allow-multiple-replacements t)
    (pushnew!
     which-key-replacement-alist
     '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1")))))
