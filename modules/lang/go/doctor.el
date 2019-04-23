;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/go/doctor.el

(unless (executable-find "guru")
  (warn! "Couldn't find guru. Refactoring commands (go-guru-*) won't work"))

(unless (executable-find "gore")
  (warn! "Couldn't find gore. REPL will not work"))

(when (featurep! :completion company)
  (require 'company-go)
  (unless (executable-find company-go-gocode-command)
    (warn! "Couldn't find gocode. Code completion won't work")))
