;;; tools/tree-sitter/autoload.el -*- lexical-binding: t; -*-

;;;###autodef (fset 'tree-sitter! #'ignore)
(defun tree-sitter! ()
  (interactive)
  (turn-on-tree-sitter-mode))

;; HACK: Remove and refactor when `use-package' eager macro expansion is solved or `use-package!' is removed
;;;###autoload
(defun +tree-sitter-get-textobj (group &optional query)
  "A wrapper around `evil-textobj-tree-sitter-get-textobj' to
prevent eager expansion."
  (eval `(evil-textobj-tree-sitter-get-textobj ,group ,query)))

;;;###autoload
(defun +tree-sitter-goto-textobj (group &optional previous end query)
  "Thin wrapper that returns the symbol of a named function, used in keybindings."
  (let ((sym (intern (format "+goto%s%s-%s" (if previous "-previous" "") (if end "-end" "") group))))
    (fset sym (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj group previous end query)))
    sym))
