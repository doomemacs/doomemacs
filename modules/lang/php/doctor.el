;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/php/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")
