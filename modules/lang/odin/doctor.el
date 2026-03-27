;;; lang/odin/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "odin")
  (warn! "Couldn't find odin in your PATH."))

(when (modulep! :editor format)
  (unless (executable-find "odinfmt")
    (warn! "Couldn't find odinfmt in PATH. Code formatting will not work.")))

(when (and (modulep! +lsp)
           (modulep! :tools lsp))
  (unless (executable-find "ols")
    (warn! "Couldn't find ols in PATH. LSP will not work.")))
