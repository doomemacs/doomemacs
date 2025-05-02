;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "588a907c87d964dbe75fba444fe65f41b4146b91")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "b97756665709bea37b9ffe262c5fa9196f1b4577")
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
