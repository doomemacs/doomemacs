;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/faust/doctor.el

(unless (executable-find "faust")
  (warn! "Couldn't find the faust compiler. faustine-mode won't work."))
