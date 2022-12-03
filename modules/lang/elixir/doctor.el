;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/elixir/doctor.el

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
