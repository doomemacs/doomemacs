;;; lang/go/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "guru")
  (warn! "Couldn't find guru. Refactoring commands (go-guru-*) won't work"))

(unless (executable-find "gore")
  (warn! "Couldn't find gore. REPL will not work"))

(when (and (featurep! :completion company)
           (require 'company-go nil t))
  (unless (executable-find command-go-gocode-command)
    (warn! "Couldn't find gocode. Code completion won't work")))
