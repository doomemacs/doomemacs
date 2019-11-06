;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/go/doctor.el

(assert! (or (not (featurep! +lsp))
             (featurep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "guru")
  (warn! "Couldn't find guru. Refactoring commands (go-guru-*) won't work"))

(unless (executable-find "gore")
  (warn! "Couldn't find gore. REPL will not work"))

(unless (executable-find "gotests")
  (warn! "Couldn't find gotests. Generating tests will not work"))

(unless (executable-find "gomodifytags")
  (warn! "Couldn't find gomodifytags. Manipulating struct tags will not work"))

(when (featurep! :completion company)
  (require 'company-go)
  (unless (executable-find company-go-gocode-command)
    (warn! "Couldn't find gocode. Code completion won't work")))
