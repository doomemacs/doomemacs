;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "c7a1f1abdbbcdb9135a73c00c58ef2f0a949f87c")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "56e92dd86b526c191275cf7813208baad14e0c5d")
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
