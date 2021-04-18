;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/nim/doctor.el

(unless (executable-find "nimsuggest")
  (warn! "Could not find nimsuggest executable; code-completion, syntax checking and jump-to-definition functionality will be disabled."))

(unless (executable-find "nim")
  (warn! "Could not find nim executable; build commands will be disabled."))

