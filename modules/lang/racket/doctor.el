;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/racket/doctor.el

(unless (executable-find "drracket")
  (warn! "Could not find drracket executable; code-completion and syntax checking will not work."))

(unless (executable-find "racket")
  (warn! "Could not find racket executable; REPL will not work."))

(unless (executable-find "raco")
  (warn! "Could not find raco executable; commands for install packages and build libraries will not work."))

(when (modulep! :editor format)
  (unless (and (executable-find "raco")
               (eq 0 (call-process-shell-command "raco fmt --help" nil nil)))
    (warn! "Couldn't find raco fmt. Formatting will be disabled.")))
