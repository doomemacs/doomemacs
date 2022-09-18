;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/java/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "javac")
  (warn! "Couldn't find the javac executable, are you sure the JDK is installed?"))

(when (modulep! :editor format)
  (unless (executable-find "clang-format")
    (warn! "Couldn't find clang-format. Code formatting will not work.")))
