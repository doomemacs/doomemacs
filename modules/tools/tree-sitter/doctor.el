;;; tools/tree-sitter/doctor.el -*- lexical-binding: t; -*-

(unless (featurep 'dynamic-modules)
  (error! "Emacs not built with dynamic modules support"))

(if (version< emacs-version "29.1")
    (error! "Emacs 29.1 or newer is required for tree-sitter support")
  (unless (treesit-available-p)
    (error! "Emacs not built with tree-sitter support!")))
