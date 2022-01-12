;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "3c88611c4ed59069093187c2a039b8d05cbe53e8")
  (when (featurep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (featurep! +lsp)
  (package! dap-mode :pin "76cad34de8984f57c2b1e374e9c985cc7ec8dad0")
  (package! posframe :pin "6c0e63d6b3b6638c11729c5db28019a38ff44f5b"))
