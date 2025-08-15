;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/cc/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(when (modulep! :completion company)
  ;; glslangValidator
  (unless (executable-find "glslangValidator")
    (warn! "Couldn't find glslangValidator. GLSL code completion is disabled")))

(when (modulep! :editor format)
  (unless (executable-find "clang-format")
    (warn! "Couldn't find clang-format. Formatting will be disabled.")))
