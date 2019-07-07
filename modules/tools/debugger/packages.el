;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud)
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni)))

(when (featurep! :tools lsp)
  (package! dap-mode))
