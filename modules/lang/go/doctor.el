;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/go/doctor.el

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(assert! (or (not (modulep! +tree-sitter))
             (modulep! :tools tree-sitter))
         "This module requires (:tools tree-sitter)")

(unless (executable-find "gore")
  (warn! "Couldn't find gore. REPL will not work"))

(unless (executable-find "gotests")
  (warn! "Couldn't find gotests. Generating tests will not work"))

(unless (executable-find "gomodifytags")
  (warn! "Couldn't find gomodifytags. Manipulating struct tags will not work"))

(when (and (modulep! :completion company)
           (modulep! -lsp))
  (require 'company-go)
  (unless (executable-find company-go-gocode-command)
    (warn! "Couldn't find gocode. Code completion won't work")))

(when (and (modulep! +lsp)
           (not (executable-find "gopls")))
  (warn! "Couldn't find gopls."))
