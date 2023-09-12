;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "220ce2c348dab6cfc1cfa3c3f59644e777f9e8ff")
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (modulep! +lsp)
  (package! dap-mode :pin "096070aacff875a09c13e596e5d60a55e0f07ab1")
  (package! posframe :pin "017deece88360c7297265680d78a0bb316470716"))
