;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/zig/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "zig")
  (warn! "Couldn't find zig binary"))

(when (featurep! +lsp)
  (unless (executable-find "zls")
    (warn! "Couldn't find zls binary")))
