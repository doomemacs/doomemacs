;;; completion/vertico/doctor.el -*- lexical-binding: t; -*-

(when (require 'consult nil t)
  ;; FIXME: This throws an error if grep is missing.
  (unless (consult--grep-lookahead-p "grep" "-P")
    (warn! "The installed grep binary was not built with support for PCRE lookaheads")
    (explain! "Some advanced consult filtering features will not work as a result, see the module readme."))

  ;; TODO: Move this to core in v3.0
  (unless (consult--grep-lookahead-p "rg" "-P")
    (warn! "The installed ripgrep binary was not built with support for PCRE lookaheads.")
    (explain! "Some advanced consult filtering features will not work as a result, see the module readme.")))
