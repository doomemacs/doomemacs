;;; tools/treesitter/doctor.el -*- lexical-binding: t; -*-

(unless (fboundp 'module-load)
  (warn! "Emacs was not built with dynamic modules support. Tree sitter needs this to function"))
