;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/odin/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(assert! (or (not (modulep! +tree-sitter))
             (fboundp 'odin-ts-mode))
         "Failed to retrieve `odin-ts-mode'")

(assert! (or (not (modulep! +lsp))
             (fboundp 'odin-mode))
         "Failed to retrieve `odin-mode'")

(unless (executable-find "odin")
  (warn! "Couldn't find odin in your PATH."))

(when (modulep! :editor format)
  (unless (executable-find "odinfmt")
    (warn! "Couldn't find odinfmt in PATH. Code formatting will not work.")))

(when (and (modulep! +lsp)
           (modulep! :tools lsp))
  (unless (executable-find "ols")
    (warn! "Couldn't find ols in PATH. LSP will not work.")))

(when (modulep! +just)
  (unless (executable-find "just")
    (warn! "Couldn't find just. just will not work.")))

(when (modulep! +tree-sitter)
    (warn! "`odin-ts-mode' is a W.I.P (Work in progress)."))
