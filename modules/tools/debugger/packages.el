;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "94f2835933")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e80")))

(when (featurep! :tools lsp)
  (package! dap-mode :pin "e2086fc9fb"))
