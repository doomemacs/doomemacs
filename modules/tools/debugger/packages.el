;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud :pin "3c88611c4ed59069093187c2a039b8d05cbe53e8")
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni :pin "0ec088ea343835e24ae73da09bea96bfb02a3130")))

(when (modulep! +lsp)
  (package! dap-mode :pin "50c2a99059da03ac5274d6794ea4758ddf253dc1")
  (package! posframe :pin "0d23bc5f7cfac00277d83ae7ba52c48685bcbc68"))
