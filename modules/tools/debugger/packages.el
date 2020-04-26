;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "b854e040e0")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "6e9cac5e80")))

(when (featurep! +lsp)
  (package! dap-mode :pin "0b9c8f28ad")
  (package! posframe :pin "e62e584268"))
