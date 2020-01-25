;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "2cca776d28")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e80")))

(when (featurep! :tools lsp)
  (package! dap-mode :pin "d10e254ce4"))
