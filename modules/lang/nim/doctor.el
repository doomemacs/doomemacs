;;; lang/nim/doctor.el

(unless (executable-find "nimsuggest")
  (warn! "Could not find nimsuggest executable; code-completion, syntax checking and jump-to-definition functionality will be disabled."))

(unless (executable-find "nim")
  (warn! "Could not find nim executable; build commands will be disabled."))

(when (modulep! :editor format)
  (unless (executable-find "nimpretty")
    (warn! "Could not find nimpretty. Formatting will be disabled.")))
