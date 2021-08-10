;;; completion/vertico/doctor.el -*- lexical-binding: t; -*-

(require 'consult)

(unless (consult--grep-lookahead-p "grep" "-P")
  (warn! "The installed grep binary was not built with support for PCRE lookaheads.
  Some advanced consult filtering features will not work as a result, see the module readme."))

(unless (consult--grep-lookahead-p "rg" "-P")
  (warn! "The installed ripgrep binary was not built with support for PCRE lookaheads.
  Some advanced consult filtering features will not work as a result, see the module readme."))
