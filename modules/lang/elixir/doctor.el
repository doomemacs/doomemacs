;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/elixir/doctor.el

(assert! (or (not (featurep! +tree-sitter))
             (featurep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")
