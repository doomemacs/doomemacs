;;; lang/ada/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "alr")
  (warn! "Alire (alr) not found in $PATH"))

(when (modulep! +lsp)
  (unless (executable-find "ada_language_server")
    (warn! "ada_language_server not found in $PATH; LSP support won't work")))
