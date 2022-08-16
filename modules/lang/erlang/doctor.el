;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/erlang/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(when (modulep! :editor format)
  (unless (executable-find "efmt")
    (warn! "Couldn't find efmt. Formatting will be disabled.")))
