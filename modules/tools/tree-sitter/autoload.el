;;; tools/tree-sitter/autoload.el -*- lexical-binding: t; -*-

;;;###autodef (fset 'tree-sitter! #'ignore)
(defun tree-sitter! ()
  "Dispatch to turn on tree sitter.

Used as a hook function which turns on `tree-sitter-mode'
and selectively turn on `tree-sitter-hl-mode'.
according to `+tree-sitter-hl-enabled-modes'"
  (turn-on-tree-sitter-mode)
  ;; conditionally enable `tree-sitter-hl-mode'
  (let ((mode (bound-and-true-p tree-sitter-hl-mode)))
    (when-let (mode (if (pcase +tree-sitter-hl-enabled-modes
                          (`(not . ,modes) (not (memq major-mode modes)))
                          ((and `(,_ . ,_) modes) (memq major-mode modes))
                          (bool bool))
                        (unless mode +1)
                      (if mode -1)))
      (tree-sitter-hl-mode mode))))

;;;###autodef (fset 'set-tree-sitter-lang! #'ignore)
(defun set-tree-sitter-lang! (mode lang)
  "Associate LANG with major MODE."
  (after! tree-sitter-langs
    (add-to-list 'tree-sitter-major-mode-language-alist (cons mode lang))))

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
