;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/racket/doctor.el

(unless (executable-find "drracket")
  (warn! "Could not find drracket executable; code-completion and syntax checking will not work."))

(unless (executable-find "racket")
  (warn! "Could not find racket executable; REPL will not work."))

(unless (executable-find "raco")
  (warn! "Could not find raco executable; commands for install packages and build libraries will not work."))
