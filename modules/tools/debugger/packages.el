;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "a2d7dc9a8dd599adf61553a73abb880d62150306")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "c81014ccc390e8994b061bd2d9f8b5dd84498a3d")
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
