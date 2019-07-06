;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! realgud)
(when (featurep! :tools lsp)
  (package! dap-mode))
