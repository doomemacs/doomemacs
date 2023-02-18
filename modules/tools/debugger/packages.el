;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "aff03aeef1e40d2abb244240bab9787f4b3e6035")
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (modulep! +lsp)
  (package! dap-mode :pin "39bfaf1a3400b3ca4e9755f4d15e33abb0dda2c4")
  (package! posframe :pin "06b939cfb06168782fc378043ff35bd7fec203b8"))
