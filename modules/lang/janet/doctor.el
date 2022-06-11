;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/janet/doctor.el

(unless (executable-find "janet")
  (warn! "Could not find janet executable; repl will not work."))
