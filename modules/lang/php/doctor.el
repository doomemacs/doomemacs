;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/php/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(when (modulep! +tree-sitter)
  (assert! (modulep! :tools tree-sitter)
           "This module requires (:tools tree-sitter)")
  (assert! (fboundp 'php-ts-mode)
           "Can't find `php-ts-mode'; Emacs 30.1+ is required")
  (unless (modulep! :lang javascript +tree-sitter)
    (error! "(:lang (javascript +tree-sitter)) required, but not enabled"))
  (unless (modulep! :lang web +tree-sitter)
    (error! "(:lang (web +tree-sitter)) required, but not enabled")))

(unless (executable-find "php")
  (warn! "Couldn't find php in your PATH"))

(unless (executable-find "composer")
  (warn! "Couldn't find composer in your PATH"))
