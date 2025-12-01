;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "5f2792ea8c4a7fd0135e6cbf5ef3d5b0c9fdc410")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "f63a301f585bcf0ab334551fc323ce06178673d0")
  (package! posframe :pin "d93828bf6c36383c365bd564ad3bab5a4403804c"))
