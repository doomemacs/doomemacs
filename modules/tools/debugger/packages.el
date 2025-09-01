;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "81ee231de1547a8f472e891c8e2499b23e2f0c42")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "c81014ccc390e8994b061bd2d9f8b5dd84498a3d")
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
