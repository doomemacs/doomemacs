;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/elixir/doctor.el

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(assert! (or (not (modulep! +tree-sitter))
             (fboundp 'elixir-ts-mode))
         "Can't find `elixir-ts-mode'; Emacs 30.1+ is required")
