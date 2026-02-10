;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "76af39b5c274642f784823aa19bc6ea47bffdd05")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "b77d9bdb15d89e354b8a20906bebe7789e19fc9b")
  (package! posframe :pin "4fc893c3c9ea3f6b5099ac1b369abb3c6da40b1e"))
