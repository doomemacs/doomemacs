;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "8dbe56d7c7557229e7fda5cca6be39dc712e72ce")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "b97756665709bea37b9ffe262c5fa9196f1b4577")
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
