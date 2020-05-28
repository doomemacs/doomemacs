;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(defvar +lsp-defer-shutdown 3
  "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer.")


;;
;;; Implementations

(if (featurep! +eglot)
    (load! "+eglot")
  (load! "+lsp"))
