;;; tools/docker/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(assert! (or (not (modulep! +tree-sitter))
             (fboundp 'dockerfile-ts-mode))
         "Can't find `dockerfile-ts-mode'; Emacs 29.1+ is required")

(when (modulep! :editor format)
  (unless (executable-find "dockfmt")
    (warn! "Couldn't find dockfmt. Formatting will be disabled.")))
