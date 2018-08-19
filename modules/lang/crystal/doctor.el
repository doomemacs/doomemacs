;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/crystal/doctor.el

(unless (executable-find "icr")
  (warn! "Couldn't find icr. REPL will not work"))
