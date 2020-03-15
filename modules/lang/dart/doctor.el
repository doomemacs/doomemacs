;;; lang/dart/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "dart")
  (warn! "Dart isn't on PATH."))

(when (featurep! +lsp)
  (require 'dart-mode)
  (unless (file-readable-p lsp-dart-sdk-dir)
    (warn! "LSP Mode can't find lsp-dart-sdk-dir.")))
