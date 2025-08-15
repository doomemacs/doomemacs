;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "411e4b6126cfbd6df9a8bdc6bff39b133a31b1d9")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "b97756665709bea37b9ffe262c5fa9196f1b4577")
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
