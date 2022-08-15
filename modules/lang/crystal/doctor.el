;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/crystal/doctor.el

(unless (executable-find "icr")
  (warn! "Couldn't find icr. REPL will not work"))

(unless (executable-find "crystal")
  (error! "Couldn't find crystal. Most language features will not work."))
