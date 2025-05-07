;;; tools/tree-sitter/doctor.el -*- lexical-binding: t; -*-

(unless (fboundp 'module-load)
  (warn! "Emacs was not built with dynamic modules support, which the treesit.el library requires"))

(unless (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  (error! "Treesit library not available. Did you build Emacs with tree-sitter support?"))
