;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "76af39b5c274642f784823aa19bc6ea47bffdd05")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "b77d9bdb15d89e354b8a20906bebe7789e19fc9b")
  (package! posframe :pin "41cc4def6190f100ba50dca51457c38f4f90dfb1"))
