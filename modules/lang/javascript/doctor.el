;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/javascript/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")
