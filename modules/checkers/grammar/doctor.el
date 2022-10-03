;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; checkers/grammar/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")
