;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(package! dape :pin "b5a4daaf1400aab35b453fe6863b1822c88647d4")

;; DEPRECATED
(when (modulep! +lsp)
  (package! dap-mode :pin "6c74027e39fca229eeb1d0d59698219ae7d0aa41")
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
