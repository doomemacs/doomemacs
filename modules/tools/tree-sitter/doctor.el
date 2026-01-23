;;; tools/tree-sitter/doctor.el -*- lexical-binding: t; -*-

(unless (bound-and-true-p module-file-suffix)  ; requires dynamic-modules support
  (error! "Emacs not built with dynamic modules support"))

(if (version< emacs-version "29.1")
    (error! "Emacs 29.1 or newer is required for tree-sitter support")
  (unless (treesit-available-p)
    (error! "Emacs not built with tree-sitter support!")))
